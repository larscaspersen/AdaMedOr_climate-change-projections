#helper functions to format downloaded ssp data
format_downloaded_ssp <-  function(downloaded){
  downloaded %>% 
    #bring stations in long format
    reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'station') %>% 
    #bring individual weather variables as columns
    reshape2::dcast(Date + model + ssp + station ~ variable, value.var = 'value') %>% 
    #add Year, Month, Day
    mutate(Year = lubridate::year(Date),
           Month = lubridate::month(Date),
           Day = lubridate::day(Date)) %>% 
    rename(Tmin = tasmin,
           Tmax = tasmax) %>% 
    relocate(Date, Year, Month, Day, model, ssp, station, Tmin, Tmax) %>% 
    #split into list of weather stations
    split(f = list(.$station)) %>% 
    purrr::map(.f = function(x) split(x, list(x$ssp, x$model)))
}

#higher-level function to create temperature scenarios
gen_rel_change_scenario <- function(downloaded_list, 
                                    weather_list,
                                    years_local_weather = NULL,
                                    times = c(2050, 2085), 
                                    baseline_year_relative_change = 2022,
                                    baseline_window_width = 15, 
                                    future_window_width = 31){
  

  #iterate over the different weather stations (locally observed data and climate change data)
  purrr::map2(downloaded_list, weather_list, function(x, y){
    
    if(is.null(years_local_weather)){
      #calculate the mean year of observation
      reference_year <- mean(c(min(y$Year), max(y$Year)))
    } else {
      reference_year <- mean(years_local_weather)
    }

    
    #run the function on the list of scenarios for one station
    out <- create_scenario_list(cmip6_one_station = x, 
                                reference_year = reference_year,
                                times = times,
                                baseline_year_relative_change = baseline_year_relative_change,
                                baseline_window_width = baseline_window_width, 
                                future_window_width = future_window_width)
  })
  
}

#lower level function to generate temperature scenario for one weather station
create_scenario_list <- function(cmip6_one_station, 
                                 reference_year,
                                 times = c(2050, 2085), 
                                 baseline_year_relative_change = 2022,
                                 baseline_window_width = 15, 
                                 future_window_width = 31){
  
  
  #iterate over the different ssp scenarios
  listoflists <- purrr::map(cmip6_one_station, function(x){
    
    #print(unique(x$ssp))
    
    #iterate over the different points of time we are interested in (2050, 2085)
    int <- purrr::map(times, function(time){
      
      #create baseline scenario (usually 2015)
      clim_senc <- chillR::temperature_scenario_from_records(weather = x, 
                                                             runn_mean = baseline_window_width,
                                                             year = baseline_year_relative_change)
      
      #create scenario for future point in time we are interested in (usually 2050 or 2085)
      clim_senc_later <- temperature_scenario_from_records(weather = x, 
                                                           runn_mean = future_window_width,
                                                           year = time)
      
      #calculate the realtive change and put that into a list, mimicking the structure in chillR
      clim_scen_adjusted <- list(data =  clim_senc_later[[1]]$data - clim_senc[[1]]$data,
                                      scenario = unique(x$ssp),
                                      start_year = time - baseline_window_width,
                                      end_year = time + baseline_window_width,
                                      scenario_year = time,
                                      reference_year = reference_year,
                                      scenario_type = 'relative',
                                      labels = unique(x$model))
      
    })
    
    #adjust names
    names(int) <- times
    
    return(int)
  })
  
  return(unlist(listoflists, recursive = FALSE))
}


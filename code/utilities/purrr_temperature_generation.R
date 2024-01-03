library(chillR)

temperature_generation_modified <- function (weather, years, sim_years, temperature_scenario = data.frame(Tmin = rep(0, 
                                                                                                                     12), Tmax = rep(0, 12)), seed = 99, check_temperature_scenario_type = TRUE, 
                                             temperature_check_args = NULL, max_reference_year_difference = 5, 
                                             warn_me = TRUE, remove_NA_scenarios = TRUE) 
{
  if (is.data.frame(temperature_scenario)) 
    temperature_scenario <- list(temperature_scenario)
  temperature_scenario_check_n_intervals <- 12
  temperature_scenario_check_check_scenario_type <- TRUE
  temperature_scenario_check_scenario_check_thresholds <- c(-5, 
                                                            10)
  temperature_scenario_check_update_scenario_type <- TRUE
  temperature_scenario_check_warn_me <- TRUE
  if (!is.null(temperature_check_args)) {
    if ("n_intervals" %in% names(temperature_check_args)) 
      temperature_scenario_check_n_intervals <- temperature_check_args$n_intervals
    if ("check_scenario_type" %in% names(temperature_check_args)) 
      temperature_scenario_check_check_scenario_type <- temperature_check_args$check_scenario_type
    if ("scenario_check_thresholds" %in% names(temperature_check_args)) 
      temperature_scenario_check_scenario_check_thresholds <- temperature_check_args$scenario_check_thresholds
    if ("update_scenario_type" %in% names(temperature_check_args)) 
      temperature_scenario_check_update_scenario_type <- temperature_check_args$update_scenario_type
    if ("warn_me" %in% names(temperature_check_args)) 
      temperature_scenario_check_warn_me <- temperature_check_args$warn_me
  }
  if (is.null(temperature_scenario)) 
    stop("No temperature scenario provided")
  if (remove_NA_scenarios) {
    scen_ok <- rep(1, length(temperature_scenario))
    for (i in 1:length(temperature_scenario)) if (length(which(is.na(temperature_scenario[[i]]$data))) > 
                                                  0) 
      scen_ok[i] <- 0
    temperature_scenario <- temperature_scenario[which(scen_ok == 
                                                         1)]
  }
  for (ts in 1:length(temperature_scenario)) {
    temperature_scenario[[ts]] <- check_temperature_scenario(temperature_scenario[[ts]], 
                                                             n_intervals = temperature_scenario_check_n_intervals, 
                                                             check_scenario_type = temperature_scenario_check_check_scenario_type, 
                                                             scenario_check_thresholds = temperature_scenario_check_scenario_check_thresholds, 
                                                             update_scenario_type = temperature_scenario_check_update_scenario_type, 
                                                             warn_me = temperature_scenario_check_warn_me)
    if (!nrow(temperature_scenario[[ts]]$data) == 12) 
      stop("This function only works with monthly temperature scenarios", 
           call. = FALSE)
  }
  if ("weather" %in% names(weather) & "QC" %in% 
      names(weather)) 
    weather <- weather$weather
  year_min <- years[1]
  year_max <- years[2]
  year_min_sim <- sim_years[1]
  year_max_sim <- sim_years[2]
  n_GPCA_iter <- 5
  n_GPCA_iteration_residuals <- 5
  p <- 1
  TEMP_MAX <- weather[, c("Month", "Day", "Year", 
                          "Tmax")]
  colnames(TEMP_MAX) <- c("month", "day", "year", 
                          "station")
  TEMP_MIN <- weather[, c("Month", "Day", "Year", 
                          "Tmin")]
  colnames(TEMP_MIN) <- c("month", "day", "year", 
                          "station")
  max_mean <- as.matrix(RMAWGEN::getMonthlyMean(TEMP_MAX, year_min, 
                                                year_max)[, 4])
  colnames(max_mean) <- "station"
  rownames(max_mean) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                          12)
  min_mean <- as.matrix(RMAWGEN::getMonthlyMean(TEMP_MIN, year_min, 
                                                year_max)[, 4])
  colnames(min_mean) <- "station"
  rownames(min_mean) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                          12)
  alldays <- make_all_day_table(tab = data.frame(Year = c(year_min, 
                                                          year_max), Month = c(1, 12), Day = c(1, 31)), no_variable_check = TRUE)
  alldays[, "YEARMODA"] <- alldays$Year * 10000 + alldays$Month * 
    100 + alldays$Day
  Tn <- TEMP_MIN
  Tn[, "YEARMODA"] <- Tn$year * 10000 + Tn$month * 100 + 
    Tn$day
  allTmins <- merge(alldays, Tn, by = "YEARMODA", all.x = TRUE)
  Tx <- TEMP_MAX
  Tx[, "YEARMODA"] <- Tx$year * 10000 + Tx$month * 100 + 
    Tx$day
  allTmaxs <- merge(alldays, Tx, by = "YEARMODA", all.x = TRUE)
  miss <- length(which(is.na(allTmins$station))) + length(which(is.na(allTmaxs$station)))
  if (!miss == 0) 
    stop("Weather record not complete for the calibration period - NULL returned", 
         call. = FALSE)
  
  simoutput <- purrr::map(1:length(temperature_scenario), function(ts){
    if (temperature_scenario[[ts]]$scenario_type == "relative") 
      if (is.na(temperature_scenario[[ts]]$reference_year)) {
        if (warn_me) 
          warning("Reference year missing - can't check if relative temperature scenario is valid", 
                  call. = FALSE)
      }
    else if (!temperature_scenario[[ts]]$reference_year == 
             median(c(year_min, year_max))) 
      stop("weather data used for calibration not valid as a baseline for this scenario ", 
           "the reference year of the scenario must correspond to the median year of the weather record ", 
           "(specified by c(year_min,year_max). At the moment, this is ", 
           median(c(year_min, year_max)), " but it should be ", 
           temperature_scenario[[ts]]$reference_year, 
           ", so that it works for this scenario", 
           call. = FALSE)
    temperatures <- temperature_scenario[[ts]]$data
    if (temperature_scenario[[ts]]$scenario_type == "relative") {
      mean_climate_Tn_sim = min_mean + temperatures$Tmin
      mean_climate_Tx_sim = max_mean + temperatures$Tmax
      if (!is.na(temperature_scenario[[ts]]$reference_year)) 
        if (!is.numeric(temperature_scenario[[ts]]$reference_year)) {
          if (warn_me) 
            warning("Reference year not numeric", 
                    call. = FALSE)
        }
      else {
        year_diff <- abs(temperature_scenario[[ts]]$reference_year - 
                           median(c(year_min, year_max)))
        if (year_diff > 0 & year_diff <= max_reference_year_difference) 
          if (warn_me) 
            warning(year_diff, " year(s) difference between reference years of the temperature scenario", 
                    " and the dataset used for calibrating the weather generator", 
                    call. = FALSE)
        if (year_diff > max_reference_year_difference) 
          stop("Difference between reference years of the temperature scenario and the dataset used for calibrating the ", 
               "weather generator greater than ", 
               max_reference_year_difference, " years (", 
               year_diff, " years) - this is too much!", 
               call. = FALSE)
      }
    }
    if (temperature_scenario[[ts]]$scenario_type == "absolute") {
      mean_climate_Tn_sim = min_mean - min_mean + temperatures$Tmin
      mean_climate_Tx_sim = min_mean - min_mean + temperatures$Tmax
      if (warn_me) 
        warning(paste("Absolute temperature scenario specified - calibration weather record only used for", 
                      "simulating temperature variation, but not for the means"), 
                call. = FALSE)
    }
    generation00 <- RMAWGEN::ComprehensiveTemperatureGenerator(station = "station", 
                                                               Tx_all = TEMP_MAX, Tn_all = TEMP_MIN, year_min = year_min, 
                                                               year_max = year_max, p = p, n_GPCA_iteration = n_GPCA_iter, 
                                                               n_GPCA_iteration_residuals = n_GPCA_iteration_residuals, 
                                                               sample = "monthly", year_min_sim = year_min_sim, 
                                                               year_max_sim = year_max_sim, mean_climate_Tn = min_mean, 
                                                               mean_climate_Tn_sim = mean_climate_Tn_sim, mean_climate_Tx = max_mean, 
                                                               mean_climate_Tx_sim = mean_climate_Tx_sim, seed = seed)
    sim <- cbind(generation00$output$Tn_gen, generation00$output$Tx_gen)
    colnames(sim) <- c("Tmin", "Tmax")
    dates <- make_all_day_table(data.frame(Year = c(year_min_sim, 
                                                    year_max_sim), Month = c(1, 12), Day = c(1, 31), 
                                           nodata = c(1, 2)), no_variable_check = TRUE)
    dates <- dates[, -6]
    return( cbind(dates, sim))
  }, .progress = TRUE)
  
  names(simoutput) <- names(temperature_scenario)
  return(gen_temps = simoutput)
}
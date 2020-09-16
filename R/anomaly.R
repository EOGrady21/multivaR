
# anomaly wrapper function to direct each data stream to correct anomaly function


#' Anomaly function wrapper
#'
#' Directs data to appropriate anomaly calculation function within multivaR
#' based on temporal and regional scales of data and metric type. All functions
#' are desinged specifically to handle data from the `azmpdata` package. If a
#' user wanted to modify these functions to use with external data sources,
#' extrame caution should be taken.
#'
#' There are multiple anomaly function available within multivaR and each should
#' be used in a different context with different data metrics.
#' The functions wrapped within this function are as follows
#' * Annual Anomaly caluclation with Annual climatology
#'      * This is used in biogeochemical metric anomaly caluclations on a
#'      regional scale for annual data
#' * Monthly anomaly with monthly climatology
#'      * This is used for some physical metrics using occupation temporal scale
#'      data or monthly data
#' * Annual anomaly from monthly climatology
#'     * This is used for some physical metrics, it first calculates a monthly
#'     anomaly and monthly climatology then aggregates the monthly data points
#'     by year to calculate annual anomalies
#' * Seasonal anomaly with seasonal climatology
#'      * Used in some biolgoical metrics such as zooplankton biomass and
#'      calanus finmarchicus abundance, requires some extra metadta to confirm
#'      seasonal distinctions (not yet implemented)
#'
#' @param data A data frame from `azmpdata`
#' @param anomalyType A character string, options 'annual', 'monthly' or
#'   'seasonal' depending on the type of anomaly the user wishes to calculate
#' @param climatologyYears a Vector of two numeric values determining the start
#'   and end years of the period which will be used to calculate a climatology
#' @param var Required only if using monthly or aggregate annual anomaly
#'   calculations, character string describing the variable for which anomaly
#'   should be calculated
#' @param normalizedAnomaly Required only if using monthly or aggregate annual
#'   anomaly calculations, logical indicating whether or not to normalize
#'   anomaly calculations
#'
#' @return
#' @export

calculate_anomaly <- function(data, anomalyType, climatologyYears, var, normalizedAnomaly){

  # check data frame input
  # check what temporal scale
  day <- grep(names(data), pattern = 'day')
  month <- grep(names(data), pattern = 'month')
  year <- grep(names(data), pattern = 'year')
  calc_type <- NULL

  if(length(day) + length(month) == 0 && anomalyType == 'annual'){
    calc_type <- 'annual'
  }
  if (length(day) + length(month) != 0 && anomalyType == 'annual'){
    calc_type <- 'ann_mon'
    if(missing(var)){
      stop('Please provide `var`, variable name for which anomaly should be calculated!')
    }
  }
  if (length(day) + length(month) != 0 && anomalyType == 'monthly'){
    calc_type <- 'monthly'
    if(missing(var)){
      stop('Please provide `var`, variable name for which anomaly should be calculated!')
    }
  }

  # not ready yet
  if(anomalyType == 'seasonal'){
    calc_type <- 'seasonal'
  }

  if(is.null(calc_type)){
    stop('Unable to determine anomaly calculation type!')
  }


  # check regions

  # annual anomaly with annual climatology
  if(calc_type == 'annual'){
  output <- annualAnomaly_byRegion(data, climatolgoyYears = climatologyYears)
  }
  # monthly anomaly with monthly climatology

  if(calc_type == 'monthly'){
    output <- monthlyAnomaly(d = data, climatologyYears = climatologyYears, var = var, normalizedAnomaly = normalizedAnomaly)
  }
  # annual anomaly from monthly climatology

  if(calc_type == 'ann_mon'){
    if(normalizedAnomaly == TRUE){
      anomaly_var <- paste0(var, '_normalizedAnomaly')
    }else{
    anomaly_var <- paste0(var, '_anomaly')
    }
    d1 <-  monthlyAnomaly(d = data, climatologyYears = climatologyYears, var = var, normalizedAnomaly = normalizedAnomaly)
    output <- annualAnomaly(d = d1, anomaly = anomaly_var)
  }

  # not ready yet
  if( calc_type == 'seasonal'){
  # seasonal anomaly seasonal climatology
    output <- seasonalAnomaly(data, climatologyYears = climatologyYears)
  }


  return(output)


}



#' Calculate seasonal anomalies
#'
#' Anomaly calculation for seasonal data from `azmpdata`
#'
#' Data frames should include a column with regional scale names, the column
#' should be named `transect`, `station`, or `area`, else the function will not
#' be able to detrmine the regional scale. The data frame should also have
#' columns `variable`, `value`, `season`, and `year`.
#'
#' @param data A data frame from `azmpdata` containing seasonal average data
#' @param climatologyYears A vector of two numbers indicating the start and end of the climatology period used to calculate anomalies
#'
#' @author Benoit Casault & Emily Chisholm
#' @return a data frame with anomaly values
#' @export
#'
#'
#'
seasonalAnomaly <- function(data, climatologyYears){

  # fix names
  transect <- grep(names(data), pattern = 'transect')
  station <- grep(names(data), pattern = 'station')
  area <- grep(names(data), pattern = 'area')

  if(length(transect) > 0){
    data <- dplyr::rename(data, region_name  = 'transect')
  }
  if(length(station) > 0){
    data <- dplyr::rename(data, region_name  = 'station')
  }
  if(length(area) > 0){
    data <- dplyr::rename(data, region_name  = 'area')
  }

  checknames <- grep(names(data), pattern = 'region_name')
  if(length(checknames) == 0){
    stop('Unable to determine regional data scale! Please ensure columns are properly named!')
  }
  ## seasonal climatology
  climatology <- data %>%
    dplyr::filter(., year>=climatologyYears[1], year<=climatologyYears[2]) %>%
    dplyr::group_by(., variable, region_name, season) %>%
    dplyr::summarise(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
    dplyr::ungroup(.)

  ## seasonal anomalies
  anomaly <- dplyr::left_join(
    data,
    climatology %>%
      dplyr::select(variable, mean, sd, region_name, season),
    by = c("variable", "region_name", "season")
  ) %>%
    dplyr::mutate(., value = (value - mean) / sd) %>%
    dplyr::select(., region_name, year, season, variable, value)

  return(anomaly)

}

# anomaly calculations
# from Chantelle Layton and Benoit Casault



# anomaly calculation Benoit style

#' Annual Anomalies by region
#'
#' This function caluclates annual anomalies ONLY FROM annual data. It can
#' accept annual data from multiple stations or sections in the same dataframe
#' although users should be cautious that sections/ stations are properly
#' labeled.
#'
#' @param data A data frame of annual data containing AT LEAST metadata columns,
#'   year and section or station as well as one or more columns of data. If
#'   gathering data from `azmpdata` package, data frame should be `...Annual_Section` or
#'   `...Annual_Station` format.
#'
#' @param climatologyYears A vector of two numbers indicating the range to be used to calculate climatology (eg. c(1999, 2015))
#' @return A data frame with annual anomalies named by the variable (NOTE: the anomalies are not named with 'anomaly', be careful of confusing with actual data values)
#' @export
#'
#' @Author Benoit Casault & Emily Chisholm
annualAnomaly_byRegion <- function(data, climatologyYears){

  # carefully check if data is section or station
  sec <- grep(names(data), pattern = 'section')
  st <- grep(names(data), pattern = 'station')
  sa <- grep(names(data), pattern = 'area')

  if(length(sec) != 0){
    type <- 'section'
  }
  if(length(st) != 0){
    type <- 'station'
  }
  if(length(sa) != 0){
    type <- 'area'
  }
  if(length(st) == 0 && length(sec) == 0 && length(sa) == 0){
    stop("Unable to determine type of data, please ensure data has metadata column 'section', 'area' or 'station'!")
  }

  # produce warning if multiple lines are found per year
  # NOTE this is a terrible check as it only checks the FIRST year in the data
  # frame and does not account for multiple rows per year across different
  # sections or stations
  yearcheck <- unique(data$year)[1]

  if(length(data[data$year == yearcheck,]) > 1){
    warning('More than one row per year of data detected, ensure your data is in the correct format!')
  }

  # find variables not in metadata consistently
  metadata_vars <- c('latitude', 'longitude', 'event_id', 'cruise_id', 'year') # add to list as necessary
  metalist <- vector()
  for(i in 1:length(data)){
    # if unique values are repeated more than 20% of the time then likely metadata
    # note this system is NOT PERFECT CAUTION
    if(length(unique(data[[i]])) < (length(data[[1]]) - length(data[[1]])*0.2)){
      warning(paste(names(data)[[i]], 'classified as metadata!'))
      metalist <- c(metalist, i)
    }
    if(names(data)[[i]] %in% metadata_vars){
      warning(paste(names(data)[[i]], 'classified as metadata!'))
      metalist <- c(metalist, i)
    }

  }
  metalist <- unique(metalist)

  variable_ind <- c(1:length(data))
  variable_ind <-variable_ind[-metalist]

  # convert data to long format
  data_long <- data %>%
    tidyr::gather(variable, value, all_of(variable_ind)) %>% # gather variables not in metadata
    dplyr::rename(region_name = all_of(type)) # name section, station or area column 'region_name' for consistent coding


  # calculate climatology
  df_climatology <- data_long %>%
    dplyr::filter(year >= climatologyYears[1] & year <= climatologyYears[2]) %>%
    dplyr::group_by(region_name, variable) %>%
    dplyr::summarise(mean=mean(value, na.rm=T), sd=sd(value, na.rm=T)) %>%
    dplyr::ungroup()

  # calculate anomalies
  df_Anomalies <- dplyr::left_join(data_long,
                                   df_climatology,
                                   by = c('region_name', "variable")) %>% # replaced hardcoded 'section' with region_name so function is more flexible (EC)
    dplyr::mutate(value = (value - mean)/sd) %>%
    dplyr::select(region_name, variable, year, value)

  # convert data to wide format
  final_df <- df_Anomalies %>%
    tidyr::spread(variable, value)

  # TODO: fix output to make variables named 'anomaly' to avoid confusion
  return(final_df)
}

# TODO WARNING if cruise spans over 2 months say september 20th - October 3rd,
# this script will calculate two monthly anomalies but it would be best for AZMP
# practices if only one monthly anomaly was calculated. workaround needs to be
# constructed for user to manually override groupings of data by month, could be
# by providing mission name and grouping by mission?
# mission info is currently included in event_id but that may change, check with Benoit



#' @title Caclulate monthly anomaly values
#'
#' @description This function calculates the monthly anomaly.
#'
#' @param d a data.frame containing at least a column named year, month, and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology.
#' @param normalizedAnomaly a logical value indicating whether or not to also calculated the
#' normalized anomaly. Default is `TRUE`.
#' @param var name of variable of which anomaly should be calculated (present in `d``)
#'
#' @return a data.frame containing columns `year`, `month`, `anomaly`, and `normalizedAnomaly`
#' if desired.
#' @family anomaly
#' @author Chantelle Layton, Emily Chisholm
#'
#' @export
#'

monthlyAnomaly <- function(d, climatologyYears, var, normalizedAnomaly = TRUE){
  mm <- monthlyClimatology(d, climatologyYears, var = var)
  months <- 1:12
  mok <- lapply(months, function(k) which(d$month == k))
  anomaly <- vector(length = length(d$year))
  for (i in mm$month){ # edited to only loop through months existing in mm
    anomaly[mok[[i]]] <- d[[var]][mok[[i]]] - mm[[var]][mm$month == i] # edited to pull exact month and not assume mm includes months 1-12
  }
  if(!normalizedAnomaly) {
    dd <- cbind(d, anomaly = anomaly)
  } else {
    dd <- cbind(monthlyNormalizedAnomaly(d, climatologyYears, var), anomaly = anomaly)
  }
 # rename anomaly vars to reflect which variable they were created from
  newname <- paste0(var, '_anomaly')
  eval(parse(text = paste('dd <- dplyr::rename(dd, ',newname,' = anomaly)')))

  normnewname <- paste0(var, '_normalizedAnomaly')
  eval(parse(text = paste('dd <- dplyr::rename(dd, ',normnewname,' = normalizedAnomaly)')))

  return(dd)
}

#' @title Calculate monthly climatology
#'
#' @description This function calculates the monthly climatology
#'
#' @param d a data.frame containing at least a column named month and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology
#'  @param var name of variable of which anomaly should be calculated (present in `d``)
#'
#' @return the results of aggregate
#'
#' @author Chantelle Layton, Emily Chisholm
#' @family anomaly
#' @importFrom stats aggregate
#'
#' @export

monthlyClimatology <- function(d, climatologyYears, var){
  okclim <- d$year >= climatologyYears[1] & d$year <= climatologyYears[2]
  dd <- d[okclim, ]
  eval(parse(text = paste('mm <- aggregate(',var,' ~ month, dd, mean, na.rm = TRUE)')))
  mm
}

#' @title Caclulate monthly normalized anomaly values
#'
#' @description This function calculates the monthly normalized anomaly.
#'
#' @param d a data.frame containing at least a column named year, month, and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology.
#' @param var name of variable of which anomaly should be calculated (present in `d``)
#'
#' @return a data.frame containing columns `year`, `month`, and `normalizedAnomaly`
#'
#' @details To calculated the normalized anomaly, two climatology values are required, those being the
#' monthly and standard deviation. The normalized anomaly values are calculated
#' by subtracting the monthly climatology value from the monthly value, and the dividing it
#' by the monthly climatology standard deviation.
#' @family anomaly
#' @author Chantelle Layton, Emily Chisholm
#'
#' @export
#'

monthlyNormalizedAnomaly <- function(d, climatologyYears, var){
  mm <- monthlyClimatology(d, climatologyYears, var = var)
  msd <- monthlyStandardDeviation(d, climatologyYears, var = var)
  months <- 1:12
  mok <- lapply(months, function(k) which(d$month == k))
  anomaly <- vector(length = length(d$year))
  for (i in mm$month){ # edit to only loop through existing months
    anomaly[mok[[i]]] <- (d[[var]][mok[[i]]] - mm[[var]][mm$month == i]) / msd[[var]][mm$month == i]
  }
  cbind(d, normalizedAnomaly = anomaly)
}


#' @title Calculate annual anomaly values
#'
#' @description This function calculates annual anomaly values from provided
#' monthly anomaly values.
#'
#' @param d a data.frame containing year, month, and at least anomaly, and optionally normalizedAnomaly
#' @param anomaly a character string naming the column of __monthly anomaly__ which should be aggregated into annual anomaly
#'
#' @return a data.frame with year, anomaly, and optionally normalizedAnomaly
#'
#' @author Chantelle Layton, Emily Chisholm
#' @family anomaly
#' @importFrom stats aggregate
#'
#' @export

annualAnomaly <- function(d, anomaly){
  anomcheck <- grep(anomaly, pattern = 'anomaly')
  if(length(anomcheck) == 0){
    warning(paste('Are you sure ', anomaly, 'is a monthly anomaly column?'))
  }
  eval(parse(text = paste('aa <- aggregate(', anomaly,' ~ year, d, mean, na.rm = TRUE)')))
  return(aa)
}

#' @title Calculate normalized annual anomaly values
#'
#' @description This function calculates normalized annual anomaly values from provided
#' monthly anomaly values.
#'
#' @param d a data.frame containing year, month, and at least anomaly
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology.
#'
#' @return a data.frame with year, normalizedAnomaly, and other columns in d
#' @family anomaly
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

annualNormalizedAnomaly <- function(d, climatologyYears){
  okclim <- d$year >= climatologyYears[1] & d$year <= climatologyYears[2]
  dd <- d[okclim, ]
  sd <- sd(dd$anomaly, na.rm = TRUE)
  normanom <- d$anomaly / sd
  cbind(d, normalizedAnomaly = normanom)
}


#' @title Calculate monthly climatology standard deviation
#'
#' @description This function calculates the monthly climatology standard deviation.
#'
#' @param d a data.frame containing at least a column named month and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#'  @param var name of variable of which anomaly should be calculated (present in `d``)
#'
#' @return the results of the aggregate function, a data.frame with columns `month` and `temperature`
#'
#' @details Monthly climatology standard deviation is calculated by taking the values within the
#' defined climatology range and finding the standard deviation for each month.
#' @family anomaly
#' @author Chantelle Layton & Emily Chisholm
#'
#' @importFrom stats sd
#' @importFrom stats aggregate
#'
#' @export

monthlyStandardDeviation <- function(d, climatologyYears, var){
  okclim <- d$year >= climatologyYears[1] & d$year <= climatologyYears[2]
  dd <- d[okclim, ]
  eval(parse(text = paste('mm <- aggregate(',var,' ~ month, dd, sd, na.rm = TRUE)')))
  mm
}



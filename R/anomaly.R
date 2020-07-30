# anomaly calculations
# from Chantelle Layton and Benoit Casault


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
#'
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
#'
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
#'
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
#' @param anomaly a character string naming the column of monthly anomaly which should be aggregated into annual anomaly
#'
#' @return a data.frame with year, anomaly, and optionally normalizedAnomaly
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

annualAnomaly <- function(d, anomaly){
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
#'
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
#'
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



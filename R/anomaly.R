# anomaly calculations
# from Chantelle Layton and Benoit Casault

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
  mm <- monthlyClimatology(d, climatologyYears)
  months <- 1:12
  mok <- lapply(months, function(k) which(d$month == k))
  anomaly <- vector(length = length(d$year))
  for (i in 1:length(months)){
    anomaly[mok[[i]]] <- d[[var]][mok[[i]]] - mm[[var]][i]
  }
  if(!normalizedAnomaly) {
    cbind(d, anomaly = anomaly)
  } else {
    cbind(monthlyNormalizedAnomaly(d, climatologyYears, var), anomaly = anomaly)
  }

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
  mm <- monthlyClimatology(d, climatologyYears)
  msd <- monthlyStandardDeviation(d, climatologyYears)
  months <- 1:12
  mok <- lapply(months, function(k) which(d$month == k))
  anomaly <- vector(length = length(d$year))
  for (i in 1:length(months)){
    anomaly[mok[[i]]] <- (d[[var]][mok[[i]]] - mm[[var]][i]) / msd[[var]][i]
  }
  cbind(d, normalizedAnomaly = anomaly)
}


#' @title Calculate annual anomaly values
#'
#' @description This function calculates annual anomaly values from provided
#' monthly anomaly values.
#'
#' @param d a data.frame containing year, month, and at least anomaly, and optionally normalizedAnomaly
#'
#' @return a data.frame with year, anomaly, and optionally normalizedAnomaly
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

annualAnomaly <- function(d){
  aa <- aggregate(anomaly ~ year, d, mean, na.rm = TRUE)
  aa
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




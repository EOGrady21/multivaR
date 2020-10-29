context('Anomaly calculations')

library(multivaR)
library(dplyr)

# create minimal data example
data <- data.frame(year = seq(1999, 2015), month = sample.int(12, 17 , replace = TRUE), ice_volume = runif(17), ice_day = runif(17), temperature = runif(17), salinity = runif(17))

annual_data <- data.frame(year = seq(1999, 2015), area = c('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd'),
                          ice_volume = runif(17), ice_day = runif(17), temperature = runif(17), salinity = runif(17))

seasonal_data <- data.frame(year = seq(1999, 2015), month = sample.int(12, 17 , replace = TRUE), area = c('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd'),
                            ice_volume = runif(17), ice_day = runif(17), temperature = runif(17), salinity = runif(17))

seasonal_data <- seasonal_data %>%
  dplyr::mutate(., season = case_when(
    month <= 6 ~ 'winter',
    month > 6 ~ 'summer'
  ))

# test 1

test_that('monthly anomalies',{
  expect_silent(dd <- monthlyAnomaly(d = data, climatologyYears = c(1999, 2010), var = 'salinity'))
  expect_true(is.data.frame(dd))
  expect_equal(length(data) +2, length(dd))
  expect_false(anyNA(dd$salinity_anomaly))

})

# test 2
test_that('multiple anomalies can be calculated',{
  expect_silent(dd <- monthlyAnomaly(d = data, climatologyYears = c(1999, 2010), var = 'salinity'))
  expect_silent(dd <- monthlyAnomaly(d = dd, climatologyYears = c(1999, 2010), var = 'temperature'))
  expect_true(is.data.frame(dd))
  expect_equal(length(data) +4, length(dd))
  expect_false(anyNA(dd$temperature_anomaly))
})

# test 3
test_that('annual anomalies',{
  expect_silent(dd <- monthlyAnomaly(d = data, climatologyYears = c(1999, 2010), var = 'salinity'))
  expect_silent(ddann <- annualAnomaly(d = dd, anomaly = 'salinity_anomaly'))
  # warning about non anomaly var
  expect_warning(ddann <- annualAnomaly(d = dd, anomaly = 'salinity'))
})

# TODO: test Benoit's Anomaly function (annualAnomaly_byRegion)

test_that('annual anomalies by region',{
  expect_warning(dd <- annualAnomaly_byRegion(data = annual_data, climatologyYears = c(1999, 2010)))
  expect_true(is.data.frame(dd))
  expect_equal(length(annual_data), length(dd))


})


# test calculation of anomalies

# test normalization


# test seasonal anomalies

test_that('seasonal anomalies',{
  expect_silent(sea_anom <- calculate_anomaly(data = seasonal_data, anomalyType = 'seasonal', climatologyYears = c(1999, 2010)))
  expect_true(is.data.frame(sea_anom))
  expect_equal(length(grep(names(sea_anom), pattern = 'season')), 1)

})


# test wrapper function
test_that('anomaly wrapper',{
  # monthly
  expect_silent(mon_anom <- calculate_anomaly(data = data, anomalyType = 'monthly', climatologyYears = c(1999, 2010), var = 'ice_volume', normalizedAnomaly = TRUE))
  expect_true(is.data.frame(mon_anom))
  expect_equal(length(names(data))+ 2, length(names(mon_anom)))

  # aggregated
  expect_silent(agg_anom <- calculate_anomaly(data = data, anomalyType = 'annual', climatologyYears = c(1999, 2010), var = 'ice_volume', normalizedAnomaly = TRUE))
  expect_true(is.data.frame(agg_anom))
  expect_equal(length(names(agg_anom)), 2)

  # annual
  expect_warning(ann_anom <- calculate_anomaly(data = annual_data, anomalyType = 'annual', climatologyYears = c(1999, 2010)))
  expect_true(is.data.frame(ann_anom))
  expect_equal(length(names(ann_anom)), length(names(annual_data)))
  expect_error(ann_anom_err <- calculate_anomaly(data = data, anomalyType = 'annual', climatologyYears = c(1999, 2010)))



})



# first round test with local files

# library(stringr)
# library(dplyr)
# source('R/anomaly.R')
#
# # test files
# fp <- 'C:/Users/chisholme/desktop/April2020- March2021/Data Access/draft_csv/'
# list_fn <- list.files(fp)
#
# # load in csv
#
# for (i in 1:length(list_fn)){
#   # isolate df name
#
#   dfname <- substr(list_fn[i], 1, nchar(list_fn[i])- 4)
#
#   eval(parse(text = paste(dfname, '<- read.csv(file.path(fp, list_fn[i]))')))
#
# }

#
# # reprex
#
# # Derived_Occupations_Sections <- read.csv('Derived_Occupations_Sections.csv')
#
# # test anomaly calcs
#
# dd <- monthlyAnomaly(d = Derived_Occupations_Sections, climatologyYears = c(1999, 2010), var = 'integrated_chlorophyll_0_100')
#
# dd <- monthlyAnomaly(d = dd, climatologyYears = c(1999, 2010), var = 'integrated_nitrate_0_50')
#
# # test annual results
#
# ddann <- annualAnomaly(d = dd, anomaly = 'integrated_chlorophyll_0_100_anomaly')
#





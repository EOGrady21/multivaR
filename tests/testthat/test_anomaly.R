context('Anomaly calculations')

# write real test with csv available in azmpdata
library(multivaR)

# create minimal data example
data <- data.frame(year = seq(1999, 2015), month = sample.int(12, 17 , replace = TRUE), ice_volume = runif(17), ice_day = runif(17), temperature = runif(17), salinity = runif(17))

annual_data <- data.frame(year = seq(1999, 2015), area = c('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd'), ice_volume = runif(17), ice_day = runif(17), temperature = runif(17), salinity = runif(17))


# test 1

test_that('monthly anomalies'{
  expect_silent(dd <- monthlyAnomaly(d = data, climatologyYears = c(1999, 2010), var = 'salinity'))
  expect_true(is.data.frame(dd))
  expect_equal(length(data) +2, length(dd))
  expect_false(anyNA(dd$salinity_anomaly))

})

# test 2
test_that('multiple anomalies can be calculated'{
  expect_silent(dd <- monthlyAnomaly(d = data, climatologyYears = c(1999, 2010), var = 'salinity'))
  expect_silent(dd <- monthlyAnomaly(d = dd, climatologyYears = c(1999, 2010), var = 'temperature'))
  expect_true(is.data.frame(dd))
  expect_equal(length(data) +4, length(dd))
  expect_false(anyNA(dd$temperature_anomaly))
})

# test 3
test_that('annual anomalies'{
  expect_silent(dd <- monthlyAnomaly(d = data, climatologyYears = c(1999, 2010), var = 'salinity'))
  expect_silent(ddann <- annualAnomaly(d = dd, anomaly = 'salinity_anomaly'))
  # warning about non anomaly var
  expect_warning(ddann <- annualAnomaly(d = dd, anomaly = 'salinity'))
})

# TODO: test Benoit's Anomaly function (annualAnomaly_byRegion)

test_that('annual anomalies by region'{
  expect_warning(dd <- annualAnomaly_byRegion(data = annual_data, climatologyYears = c(1999, 2010)))
  expect_true(is.data.frame(dd))
  expect_equal(length(annual_data), length(dd))


})


# test calculation of anomalies

# test normalization



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





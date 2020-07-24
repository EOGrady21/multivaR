# query test

context('Test azmpdata querying')

library(multivaR)
library(azmpdata)

# NOTE this test may need to be adjusted as more dataframes are added to azmpdata
test_that('chlorophyll example', {

  ans <- query(search_key = c('mean', 'chlorophyll'), location = 'metadata')
  expect_true(length(ans) == 1)
  expect_true(ans == "chlorophyll_inventory_annual_means_hl2")

})

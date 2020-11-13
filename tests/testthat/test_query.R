# query test

context('Test azmpdata querying')

library(multivaR)
devtools::install_github("casaultb/azmpdata")
library(azmpdata)

# NOTE this test may need to be adjusted as more dataframes are added to azmpdata
test_that('search example', {

  ans <- query(search_key = c('annual', 'broadscale', 'derived'), location = 'title')
  expect_true(length(ans) == 1)
  expect_true(ans == "Derived_Annual_Broadscale")

})

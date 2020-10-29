# PCA testing

context('Test PCA results')

# create sample data
data <- data.frame(year = seq(1999, 2015), ice_volume = runif(17), ice_day = runif(17), temperature = runif(17), salinity = runif(17))
data$ice_volume[c(15, 2, 8)] <- -99
data <- handle_na(data, na_value = -99, col = 'ice_volume')

test_that('PCA results handle NAs properly' , {
  res <- PCA(data)
  expect_true(length(res$x$year) == 14)
})

test_that('PCA handles data.frames and data.tables', {

  dt <- data.table::as.data.table(data)
  resdt <- PCA(dt)

  res <- PCA(data)

  expect_identical(resdt, res)
})

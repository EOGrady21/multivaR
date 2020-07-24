
context('test NA handling')

library(multivaR)

# set up data

data <- data.frame(year = seq(1999, 2015), ice_volume = runif(17))
data$ice_volume[c(15, 2, 8)] <- -99
cn <- 'ice_volume'

# test 1
test_that('Test info is printed',{

expect_message(handle_na(data, na_value = -99, col = cn))

})

# test 2
test_that('Proper values are NAd',{
  data <- handle_na(data, na_value = -99, col = cn)

  expect_true(length(data[[cn]][is.na(data[[cn]])]) == 3)
})

# NA handling
# July 2020
# Emily Chisholm

# It is common in metric data (especially historical) for -99 to represent missing values
# this should be changed to NA to be properly handled in R


#' Handle missing values
#' Adjust missing values in data frame (typically -99) to NA so they can be
#' properly handled in further operations. Note this operation inserts NA's into
#' the entire row in which there is a missing value.
#'
#' @param data data frame or data table with missing values
#' @param na_value The 'missing' value which will be translated to NA within
#'   `data` (typically -99)
#'  @param col Which column within `data` should be checked for missing values
#'
#' @return
#' @export
#'
#' @examples
handle_na <- function(data, na_value, col){
  message(paste(length(data[data == na_value]), 'values adjusted to NA!'))
  data[data[[col]] == na_value,] <- NA
  return(data)
}

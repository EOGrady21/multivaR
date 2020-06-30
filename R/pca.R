# PCA function from Dan Reed, modified by E. Chisholm June 2020

#' Principal component analysis for given set of variables
#'
#'
#' @param data a datatable or dataframe with varibles to be analyzed for PCA
#'
#' @return
#' @export
#'
#' @examples
PCA <- function(data){

  # convert to datatable if necessary
  if(is.data.frame(data)){
    data <- as.data.table(data)
  }

  # Remove incomplete rows & and duplicate observations
  dt <- na.omit(unique(data))

  # Perform PCA for given months, station and variables
  res <- prcomp(x = na.omit(dt[, -"year"]), scale. = TRUE, center = TRUE)

  # Add time to the scores for plotting purposes
  res$x <- data.table(year = dt$year, res$x)

  return(res)
}

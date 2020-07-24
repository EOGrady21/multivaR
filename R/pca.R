# PCA function from Dan Reed, modified by E. Chisholm June 2020

#' Principal component analysis for given set of variables
#' Performs a principal components analysis using the stats package, on all
#' variables provided in `data` with the exception of `year`. Converts to
#' data.table and removes duplicate or incomplete rows before processing.
#'
#' @param data a datatable or dataframe with varibles to be analyzed for PCA
#'
#' @author Daniel Reed, Emily Chisholm
#' @return
#' @export
#'
#' @examples
PCA <- function(data){

  # convert to datatable if necessary
  if(!is.data.table(data)){
    data <- as.data.table(data)
  }

  # Remove incomplete rows & and duplicate observations
  dt <- na.omit(unique(data))

  # Perform PCA for given months, station and variables
  res <- stats::prcomp(x = na.omit(dt[, -"year"]), scale. = TRUE, center = TRUE)

  # Add time to the scores for plotting purposes
  res$x <- data.table(year = dt$year, res$x)

  return(res)
}

# PCA function from Dan Reed, modified by E. Chisholm June 2020

#' Principal component analysis for given set of variables
#' Performs a principal components analysis using the stats package, on all
#' variables provided in `data`. Converts to
#' data.table and removes duplicate or incomplete rows before processing.
#'
#' @param data a datatable or dataframe with varibles to be analyzed for PCA, note the dat atable provided should be a subset excluding any metadata columns
#' @importFrom data.table is.data.table as.data.table data.table
#' @importFrom stats na.omit
#' @author Daniel Reed, Emily Chisholm
#'
#'
#' @export
PCA <- function(data){

  # convert to datatable if necessary
  if(!data.table::is.data.table(data)){
    data <- data.table::as.data.table(data)
  }

  # Remove incomplete rows & and duplicate observations
  dt <- stats::na.omit(unique(data))

  # remove metadata columns
  # TODO


  # Perform PCA for given months, station and variables
  res <- stats::prcomp(x = na.omit(dt), scale. = TRUE, center = TRUE)

  # Add time to the scores for plotting purposes
  res$x <- data.table::data.table(year = dt$year, res$x)

  return(res)
}

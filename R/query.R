# Query azmpdata

# query azmp data

query <- function(search_key, location = 'title'){
  #' Query azmpdata Data products
  #'
  #' Search for matches to search term(s) within azmpdata package data objects. Other search functions that may be useful include
  #'  azmpdata::lookup_variable (which allows searching of the detailed variable table - including descriptions of all variables)
  #'  or azmpdata::variable_lookup (which also searches through dataframe titles)
  #'
  #' @param search_key Keyword(s) to search for in azmpdata, if using multiple keywords, provide vector of character strings
  #' @param location Where to search for keyword(s), options are 'title' (which searches through the titles of data frames), or 'metadata' (which searches through the descriptions of dataframes)
  #'
  #' @importFrom utils installed.packages
  #' @note Leave search_key blank `''` to return all available datasets
  #' @examples
  #' \dontrun{
  #' devtools::install_github("casaultb/azmpdata")
  #' library(azmpdata)
  #' query(search_key = c('broadscale'), location = 'title')
  #'}
  #'
  #' @family search
  #' @export

  # confirm search location
  search_locs <- c('title', 'metadata')
  if(!location %in% search_locs){
    stop('Invalid location argument!')
  }

  # check for azmpdata
  if('azmpdata' %in% rownames(utils::installed.packages()) == FALSE){
    stop('Please install azmpdata package before attempting to query!')
  }
  rda_list <- utils::data(package="azmpdata")

  # search titles
  if(location == 'title'){
    titles <- rda_list$results[,3]

    res <- list()
    for(i in 1:length(search_key)){
      res[[i]] <- grep(titles, pattern = search_key[i], value = TRUE, ignore.case = TRUE)
    }

    vals <- unique(unlist(res)) #  unique titles which match any search terms
    all_vals <- unlist(res) # all titles which match any search terms
    names(res) <- search_key

    # count how many times title is repeated
    rep <- list()
    for(v in vals){
      rep[[v]] <- grep(pattern = v, x = all_vals)
    }

    # if title is matched to every search term, success
    final <- list()
    for(i in 1:length(rep)){
      if(length(rep[[i]]) == length(search_key)){
        final[[i]] <- names(rep[i])
      }
    }

    # print only titles matching ALL search terms
    return(unlist(final))

  }

  # search metadata (description)
  # TODO add other metadta search fields
  if(location == 'metadata'){
    titles <- rda_list$results[,3]
    desc <- rda_list$results[,4]

    res <- list()
    for(i in 1:length(search_key)){
      res[[i]] <- grep(desc, pattern = search_key[i], value = TRUE, ignore.case = TRUE)
    }

    vals <- unique(unlist(res)) #  unique desc which match any search terms
    all_vals <- unlist(res) # all desc which match any search terms
    names(res) <- search_key

    # count how many times desc is repeated
    rep <- list()
    for(v in vals){
      rep[[v]] <- grep(pattern = v, x = all_vals)
    }

    # if desc is matched to every search term, success
    final <- list()
    for(i in 1:length(rep)){
      if(length(rep[[i]]) == length(search_key)){
        final[[i]] <- names(rep[i])
      }
    }

    # match desc to title
    final_tit <- titles[which(desc == final)]

    # print only titles matching ALL search terms
    return(unlist(final_tit))

  }




}

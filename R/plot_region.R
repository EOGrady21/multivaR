# regional polygon visualization
# e. chisholm
# august 2020

# library(xlsx)
# library(dplyr)
# library(oce)

# example 1

# read in polygon table
# regtab <- read.xlsx('C:/Users/ChisholmE/Desktop/April2020- March2021/Data Access/azmpmetrics_v2.xlsx', sheetName = 'Regional look up table')

# save into package for example

#write.csv(regtab, file = 'inst/extdata/regional_look_up.csv', row.names = FALSE)

# attempt to plot

#' Plot a data region [UNDER CONSTRUCTION]
#'
#'
#' @param name name of the region to visualize
#' @param plot Logical value, if FALSE, will output data frame of coordinates
#'   which can be plotted by user
#' @param longitudelim optional vector of two numbers describing longitude
#'   limits of plotting window (passed to /code{/link{oce::mapPlot}}), if no
#'   value is given uses a maritimes centred default
#' @param latitudelim optional vector of two numbers describing latitude limits
#'   of plotting window (passed to /code{/link{oce::mapPlot}}), if no value is
#'   given uses a maritimes centred default
#'
#'
#' @export
#'
#'
plot_region <- function(name, longitudelim, latitudelim, plot = TRUE){
  # get table
  sysreg_att<- system.file('extdata/', 'regional_attributes.csv', package = 'multivaR', mustWork = TRUE) # will these files actually be in azmpdata?
  sysreg_geo<- system.file('extdata/', 'regional_geometry.csv', package = 'multivaR', mustWork = TRUE)

  regtab_att <- read.csv(sysreg)
  regtab_geo <- read.csv(sysreg)


  # find region to plot
  subtab_att <- regtab_att[regtab_att$name == name,] # check for short and long names

  if(length(subtab_att[[1]]) == 0){
    stop('No data found for region specified!')
  }
  # check type
  type <- unique(subtab_att$type)
  if (length(type) > 1){
    stop('Attempting to plot multiple types of regions!')
  }

  # get record ID for named region
  id <- unique(subtab_att$record_id)


  # get geo data matching record ID

  subtab_geo <- regtab_geo[regtab_geo$record_id == id,]



  # set general region to plot
  # maritimes default
  if(missing(longitudelim)){
  longitudelim <- c(-70, -55)
  }
  if(missing(latitudelim)){
  latitudelim <- c(50, 40)
  }

if (plot == TRUE){
  oce::mapPlot( longitudelim = longitudelim, latitudelim = latitudelim , border = 'green', type = 'polygon')
  par(new = TRUE)
  oce::mapPlot(longitude = subtab$longitude, latitude = subtab$latitude, type = 'o',  longitudelim = longitudelim, latitudelim = latitudelim)

}else{
  # plot == FALSE
  return(subtab)
}

}


# EXAMPLE 2
# using Benoit's tables

# get tables (x can't download)
# imitate tables from local csv

# get table
# sysreg<- system.file('extdata/', 'regional_look_up.csv', package = 'multivaR', mustWork = TRUE)
# regtab <- read.csv(sysreg)
#
# stationtab <- regtab[regtab$type == 'station',]
#
# nafotab <- regtab[regtab$type == 'NAFO', ]
#
# pettab <- regtab[regtab$type == 'Petrie Box', ]
#
# write.csv(stationtab, file = 'inst/extdata/station_regional_look_up.csv', row.names = FALSE)
# write.csv(nafotab, file = 'inst/extdata/nafo_regional_look_up.csv', row.names = FALSE)
# write.csv(pettab, file = 'inst/extdata/pet_regional_look_up.csv', row.names = FALSE)
#
#' Plot a data region
#'
#' USES BENOIT'S DATA FORMAT, SEPERATE TABLES FOR EACH TYPE OF REGION
#'
#'  @param name name of the region to visualize
#' @param plot Logical value, if FALSE, will output data frame of coordinates
#'   which can be plotted by user
#' @param longitudelim optional vector of two numbers describing longitude
#'   limits of plotting window (passed to /code{/link{oce::mapPlot}}), if no
#'   value is given uses a maritimes centred default
#' @param latitudelim optional vector of two numbers describing latitude limits
#'   of plotting window (passed to /code{/link{oce::mapPlot}}), if no value is
#'   given uses a maritimes centred default
#'
#'
#'
#'
# plot_region_BC <- function(name, longitudelim, latitudelim, plot = TRUE){
#
#   # gather all regional look up tables
#   extdat_path <- system.file('extdata', package = 'multivaR', mustWork = TRUE)
#   data_files <- list.files(extdat_path, full.names = TRUE)
#
#   # find regional tables
#   reg_tables <- grep(data_files, pattern = '*regional_look_up.csv', value = TRUE)
#
#   # WARNING !! FOR TESTING PLEASE REMOVE
#   # remove other format table for testing
#   reg_tables <- reg_tables[-3]
#   # !!!!
#
#   # find named region for plotting
#   plottable <- NULL
#   while(is.null(plottable)){
#   for(i in 1:length(reg_tables)){
#       tab <- read.csv(reg_tables[[i]])
#       named <- grep(tab, pattern = name)
#       if(length(named) != 0){
#         plottable <- tab
#       }
#   }
#     break
#   }
#
#   if(is.null(plottable)){
#     stop('No data found for region specified!')
#   }
#
#   # find specific lines for plotting
#
#   subtab <- plottable[plottable$name == name, ]
# if(plot == TRUE){
#   # plot
#   # set general region to plot
#   # maritimes default
#   if(missing(longitudelim)){
#     longitudelim <- c(-70, -55)
#   }
#   if(missing(latitudelim)){
#     latitudelim <- c(50, 40)
#   }
#
#   oce::mapPlot( longitudelim = longitudelim, latitudelim = latitudelim , border = 'green', type = 'polygon')
#   par(new = TRUE)
#   oce::mapPlot(longitude = subtab$longitude, latitude = subtab$latitude, type = 'o',  longitudelim = longitudelim, latitudelim = latitudelim)
# }else{
#   return(subtab)
# }
#
#
# }

# read in Benoit's tables

# fp <- 'C:/Users/ChisholmE/Desktop/April2020- March2021/Data Access/Benoit_sample_tables/'
#
# sampfiles <- list.files(fp,  pattern = '.csv')
#
# for(i in 1:length(sampfiles)){
#   dfname <- gsub('(\\w+)\\.csv', '\\1', sampfiles[i]) # thanks CL for the reg expression!
#
#   eval(parse(text = paste(dfname, '<- read.csv(file.path(fp, sampfiles[i]))')))
#
# }



# uses BC's actual sample format with a names tables and a coords table
# plot_region_BC2 <- function(name, longitudelim, latitudelim, plot = TRUE){
#
#   # gather all regional look up tables
#   extdat_path <- system.file('extdata/regional_tables', package = 'multivaR', mustWork = TRUE)
#   data_files <- list.files(extdat_path, full.names = TRUE, pattern = '.csv')
#
#   name_data <- grep(data_files, pattern = 'names', ignore.case = TRUE, value = TRUE)
#
#
#   # find named region for plotting
#   nametable <- NULL
#   while(is.null(nametable)){
#     for(i in 1:length(name_data)){
#       tab <- read.csv(name_data[[i]])
#       named <- grep(tab, pattern = name)
#       if(length(named) != 0){
#         nametable <- tab
#         fn <- name_data[[i]]
#       }
#     }
#     break
#   }
#
#   if(is.null(nametable)){
#     stop('No data found for region specified!')
#   }
#
#   # find 'record' matching to name
#
#   col_id <- grep(nametable, pattern = name)
#   row_id <- grep(nametable[[col_id]], pattern = name)
#   record_iden <- nametable$record[row_id]
#
#   # find data file which matches name file
#   # CAUTION naming needs to be consistent for this to work, checks before underscore for indentical name in data and name file
#   # THIS IS GARBAGE AND EASILY BROKEN
#   fn2 <- str_split(fn, '/')
#   fn3 <- fn2[[1]][length(fn2[[1]])]
#   areaname <- str_extract(fn3, "[^_]+") # WARNING THIS ONLY PULLS BEFORE FIRST UNDERSCORE TO IDENTIFY DATA TABLE, REQUIRES NEW AND CONSISTENT NAMING SCHEME
#   datatable <- grep(data_files, pattern = areaname, value = TRUE)
#   datatable2 <- grep(datatable, pattern = 'Coordinates', value = TRUE)
#   # !!! SOS !!!
#
#
#   plottable <- read.csv(datatable2, stringsAsFactors = FALSE)
#
#   # find specific lines for plotting
#
#   subtab <- plottable[plottable$record == record_iden, ]
#   if(plot == TRUE){
#     # plot
#     # set general region to plot
#     # maritimes default
#     if(missing(longitudelim)){
#       longitudelim <- c(-70, -55)
#     }
#     if(missing(latitudelim)){
#       latitudelim <- c(50, 40)
#     }
#
#     oce::mapPlot( longitudelim = longitudelim, latitudelim = latitudelim , border = 'green', type = 'polygon')
#     par(new = TRUE)
#     oce::mapPlot(longitude = subtab$longitude, latitude = subtab$latitude, type = 'o',  longitudelim = longitudelim, latitudelim = latitudelim)
#   }else{
#     return(subtab)
#   }
#
#
# }

#
# # test timing
# #
# EC <- list()
# for(i in 1:100){
#
# start <- Sys.time()
# plot_region(name = 'HL2')
# end <- Sys.time()
#
# EC[[i]] <- end-start
# print(paste(i , '/ 100 EC reps'))
# }
# ec_avg <- mean(as.numeric(EC))
#
#
# BC <- list()
# for(i in 1:100){
# start <- Sys.time()
# plot_region_BC(name = 'HL2')
# end <- Sys.time()
#
# BC[[i]] <- end - start
# print(paste(i , '/ 100 BC1 reps'))
#
# }
# bc_avg <- mean(as.numeric(BC))
#
# BC2 <- list()
# for(i in 1:100){
#   start <- Sys.time()
#   plot_region_BC2(name = 'HL2')
#   end <- Sys.time()
#
#   BC2[[i]] <- end - start
#   print(paste(i , '/ 100 BC2 reps'))
#
# }
# bc2_avg <- mean(as.numeric(BC2))


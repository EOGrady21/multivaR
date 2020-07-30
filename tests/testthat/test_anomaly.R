context('Anomaly calculations')

# write real test with csv available in azmpdata

# test format

# test calculation of anaomalies

# test normalization



# first round test with local files

library(stringr)
library(dplyr)
source('R/anomaly.R')

# test files
fp <- 'C:/Users/chisholme/desktop/April2020- March2021/Data Access/draft_csv/'
list_fn <- list.files(fp)

# load in csv

for (i in 1:length(list_fn)){
  # isolate df name

  dfname <- substr(list_fn[i], 1, nchar(list_fn[i])- 4)

  eval(parse(text = paste(dfname, '<- read.csv(file.path(fp, list_fn[i]))')))

}


# reprex

# Derived_Occupations_Sections <- read.csv('Derived_Occupations_Sections.csv')

# test anomaly calcs

dd <- monthlyAnomaly(d = Derived_Occupations_Sections, climatologyYears = c(1999, 2010), var = 'integrated_chlorophyll_0_100')

dd <- monthlyAnomaly(d = dd, climatologyYears = c(1999, 2010), var = 'integrated_nitrate_0_50')

# test annual results

ddann <- annualAnomaly(d = dd, anomaly = 'integrated_chlorophyll_0_100_anomaly')


# TODO: does data need to be grouped by section/ station before calculating anomaly?


# NOTE: Benoit's version calculates a very different result than Chantelle's, I suspect the implementation of Benoit's code is incorrectly implemented


# Benoit's version -----------------------
df_data_filtered_l <- Derived_Occupations_Sections

df_data_filtered_l <- df_data_filtered_l %>%
  select(., section, station, latitude, longitude, year, month, day, event_id, integrated_chlorophyll_0_100)

##--------------------------------------------------------------------------------------------
## annual climatology
df_climatology_annual_l <- df_data_filtered_l %>%
  dplyr::filter(., year>=1999, year<=2010) %>%
  dplyr::group_by(., section, year) %>%
  dplyr::summarise(., mean=mean(integrated_chlorophyll_0_100, na.rm=TRUE), sd=sd(integrated_chlorophyll_0_100, na.rm=TRUE)) %>%
  dplyr::ungroup(.)



  ##--------------------------------------------------------------------------------------------
## annual anomalies
df_anomaly_annual_l <- dplyr::left_join(df_data_filtered_l,
                          df_climatology_annual_l ,
                          by=c("section", "year")) %>%
  dplyr::group_by(., year) %>%
  dplyr::mutate(., value=(integrated_chlorophyll_0_100-mean)/sd) %>%
  summarise(., annual_anomaly = mean(value, na.rm = TRUE)) %>%
  dplyr::select(., year,  annual_anomaly) %>%
  dplyr::ungroup(.)



# check for NAs

anyNA(Derived_Occupations_Sections$integrated_chlorophyll_0_100)

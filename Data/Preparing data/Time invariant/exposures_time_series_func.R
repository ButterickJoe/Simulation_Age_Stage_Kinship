
library(future)
library(foreach)
plan("multisession" , workers =  8)

time_series_exposures <- function(lists_of_years_of_exposures){
  
  list_of_years_needed1 <- lists_of_years_of_exposures[[1]]
  list_of_years_needed2 <- lists_of_years_of_exposures[[2]]
  list_of_years_needed3 <- lists_of_years_of_exposures[[3]]
  list_of_years_needed4 <- lists_of_years_of_exposures[[4]]
  list_of_years_needed5 <- lists_of_years_of_exposures[[5]]
  
############# Time series of population exposures from 2016 up to 2021
#################

time_dep_pop_df_2016_2021 <- list()
foreach(year = list_of_years_needed1)%do%{
  test_data_pop_counts <- readxl::read_xlsx(here::here("Data", "Demogaphic data" , "nomis_2016_2021.xlsx" ),
                                            sheet = paste(year), skip=5)

  colnames(test_data_pop_counts) <- c("LAD" , "CODE" ,"0_4","5_9","10_14","15_19",
                                    "20_24","25_29","30_34","35_39","40_44",
                                    "45_49","50_54","55_59","60_64","65_69",
                                    "70_74","75_79","80_84","85_89")

  test_data_pop_counts$Year <- year
  test_data_pop_counts$Year <- as.numeric(test_data_pop_counts$Year)
  time_dep_pop_df_2016_2021[[(1+length(time_dep_pop_df_2016_2021))]] <- test_data_pop_counts
}
full_pop_counts_2016_2021 <- do.call("rbind" , time_dep_pop_df_2016_2021)

############# Time series of population exposures from 2009 up to 2015

time_dep_pop_df_2009_2015 <- list()
foreach(year = list_of_years_needed2)%do%{
  test_data_pop_counts <- readxl::read_xlsx(here::here("Data", "Demogaphic data", "nomis_2009_2015.xlsx" ),
                                            sheet = paste(year), skip=5)
  colnames(test_data_pop_counts) <- c("LAD" , "CODE" ,"0_4","5_9","10_14","15_19",
                                      "20_24","25_29","30_34","35_39","40_44",
                                      "45_49","50_54","55_59","60_64","65_69",
                                      "70_74","75_79","80_84","85_89")
  test_data_pop_counts$Year <- year
  test_data_pop_counts$Year <- as.numeric(test_data_pop_counts$Year)
  time_dep_pop_df_2009_2015[[(1+length(time_dep_pop_df_2009_2015))]] <- test_data_pop_counts
}
full_pop_counts_2009_2015 <- do.call("rbind" , time_dep_pop_df_2009_2015)
############# Time series of population exposures from 2002 up to 2008

time_dep_pop_df_2002_2008 <- list()
foreach(year = list_of_years_needed3)%do%{
  test_data_pop_counts <- readxl::read_xlsx(here::here("Data", "Demogaphic data", "nomis_2002_2008.xlsx" ),
                                            sheet = paste(year), skip=5)
  
  colnames(test_data_pop_counts) <- c("LAD" , "CODE" ,"0_4","5_9","10_14","15_19",
                                      "20_24","25_29","30_34","35_39","40_44",
                                      "45_49","50_54","55_59","60_64","65_69",
                                      "70_74","75_79","80_84","85_89")
  test_data_pop_counts$Year <- year
  test_data_pop_counts$Year <- as.numeric(test_data_pop_counts$Year)
  time_dep_pop_df_2002_2008[[(1+length(time_dep_pop_df_2002_2008))]] <- test_data_pop_counts
}
full_pop_counts_2002_2008 <- do.call("rbind" , time_dep_pop_df_2002_2008)

############# Time series of population exposures from 2002 up to 2008

time_dep_pop_df_1995_2001 <- list()
foreach(year = list_of_years_needed4)%do%{
  test_data_pop_counts <- readxl::read_xlsx(here::here("Data", "Demogaphic data", "nomis_1995_2001.xlsx" ),
                                            sheet = paste(year), skip=5)
  
  colnames(test_data_pop_counts) <- c("LAD" , "CODE" ,"0_4","5_9","10_14","15_19",
                                      "20_24","25_29","30_34","35_39","40_44",
                                      "45_49","50_54","55_59","60_64","65_69",
                                      "70_74","75_79","80_84","85_89")
  test_data_pop_counts$Year <- year
  test_data_pop_counts$Year <- as.numeric(test_data_pop_counts$Year)
  time_dep_pop_df_1995_2001[[(1+length(time_dep_pop_df_1995_2001))]] <- test_data_pop_counts
}
full_pop_counts_1995_2001 <- do.call("rbind" , time_dep_pop_df_1995_2001)

############# Time series of population exposures from 1991 up to 1994

time_dep_pop_df_1991_1994 <- list()
foreach(year = list_of_years_needed5)%do%{
  test_data_pop_counts <- readxl::read_xlsx(here::here("Data", "Demogaphic data", "nomis_1991_1994.xlsx" ),
                                            sheet = paste(year), skip=5)
  colnames(test_data_pop_counts) <- c("LAD" , "CODE" ,"0_4","5_9","10_14","15_19",
                                      "20_24","25_29","30_34","35_39","40_44",
                                      "45_49","50_54","55_59","60_64","65_69",
                                      "70_74","75_79","80_84","85_89")
  test_data_pop_counts$Year <- year
  test_data_pop_counts$Year <- as.numeric(test_data_pop_counts$Year)
  time_dep_pop_df_1991_1994[[(1+length(time_dep_pop_df_1991_1994))]] <- test_data_pop_counts
}

full_pop_counts_1991_1994 <- do.call("rbind" , time_dep_pop_df_1991_1994)


############################### FULL DATA FROM 1991 - 2021 ################

full_pop_counts_1991_2021 <- rbind(full_pop_counts_1991_1994,
                                   full_pop_counts_1995_2001,
                                   full_pop_counts_2002_2008,
                                   full_pop_counts_2009_2015,
                                   full_pop_counts_2016_2021)

full_pop_counts_1991_2021 <- full_pop_counts_1991_2021%>%dplyr::select(-"LAD")

return(full_pop_counts_1991_2021)
}

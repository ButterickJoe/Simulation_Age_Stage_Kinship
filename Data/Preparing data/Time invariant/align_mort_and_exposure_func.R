

align_mort_exposures <- function(deaths_df, exposures_df){


############# Import the data set of deaths -- deaths_df
############# time : 1981 to 2021 
############# regions : LADs 
############# ages : 5 year gaps, with infant "<1" and then +5 up to 90-94
  
############ Import the exposures data, from 1991 to 2021 
############ LADs and 5yr ages
############ This data set is created using the function "exposures_time_series_func.R
  
full_pop_counts_1991_2021 <- exposures_df
full_pop_counts_1991_2021$`90_94` <- as.numeric(full_pop_counts_1991_2021$`85_89`)/5
full_pop_counts_1991_2021$`85_89` <- as.numeric(full_pop_counts_1991_2021$`85_89`)/3



test_data_deaths <- deaths_df

### Create a list of the unique LADs by "code" -- E****** or "*******
deaths_LAD_list <- test_data_deaths$`LA code`%>%unique()
deaths_place_list <- test_data_deaths$`LA name`%>%unique()
### Extract the column names of data frame, for use when joining with counts
names_to_sum <- colnames(test_data_deaths)



colnames(test_data_deaths) <- c("Year", "CODE", "LA_name", "Combined_ages" , "under_1",
                                "1_4","5_9","10_14","15_19",
                                 "20_24","25_29","30_34","35_39","40_44",
                                 "45_49","50_54","55_59","60_64","65_69",
                                 "70_74","75_79","80_84","85_89","90_94","95+")

## relabel columns to remove the place names and total deaths, and restrict to years where we have counts
test_data_deaths_join <- test_data_deaths%>%dplyr::select(c("Year", "CODE", "under_1" ,
                                                            "1_4","5_9","10_14","15_19",
                                                            "20_24","25_29","30_34","35_39","40_44",
                                                            "45_49","50_54","55_59","60_64","65_69",
                                                            "70_74","75_79","80_84","85_89","90_94","95+"))%>%
  filter(Year < 2022 , Year > 1990)

## combine infant mortality with 1-4 year olds (so it's a 5 year class)
test_data_deaths_join$`1_4` <- test_data_deaths_join$under_1 + test_data_deaths_join$`1_4`
test_data_deaths_join$`90_94` <-  test_data_deaths_join$`90_94` + test_data_deaths_join$`95+`
test_data_deaths_join <- test_data_deaths_join%>%dplyr::select(-c(3,23))
test_data_deaths_join <- test_data_deaths_join%>%dplyr::rename(c(`0_4` = `1_4`))

deaths_LAD_list <- test_data_deaths_join$CODE%>%unique()
counts_LAD_list <- full_pop_counts_1991_2021$CODE%>%unique()
both_LADs <- intersect(deaths_LAD_list,counts_LAD_list)

test_data_deaths_join <- test_data_deaths_join%>%filter(CODE%in%both_LADs)
full_pop_counts_1991_2021 <- full_pop_counts_1991_2021%>%filter(CODE%in%both_LADs)


full_pop_counts_1991_2021 <- full_pop_counts_1991_2021%>%as.data.frame()
test_data_deaths_join <- test_data_deaths_join%>%as.data.frame()

DF_counts <- full_pop_counts_1991_2021%>%melt(id = c("CODE","Year"))%>%
  mutate(Age = variable,
         counts = value,
         CODE = CODE,
         Year = Year)%>%dplyr::select(c("Age","counts","CODE","Year"))

DF_deaths <- test_data_deaths_join%>%melt(id = c("Year","CODE"))%>%
  mutate(Age = variable,
         deaths = value,
         CODE = CODE,
         Year = Year)%>%dplyr::select(c("Age","deaths","CODE","Year"))


full_df <- full_join(DF_deaths,DF_counts,by = c("Age" , "CODE" , "Year"))

full_df$deaths <- as.numeric(full_df$deaths)
full_df$counts <- as.numeric(full_df$counts)

return(list(full_df,DF_counts,DF_deaths))
}

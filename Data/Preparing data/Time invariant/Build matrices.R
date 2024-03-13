no_age <-19
no_stage <- 7

source(here::here("Data", "Preparing data" , "Time invariant" , "exposures_time_series_func.R"))
source(here::here("Data", "Preparing data" , "Time invariant", "align_mort_and_exposure_func.R"))
source(here::here("Data", "Preparing data" , "Time invariant", "align_mig_clusters_with_fertmort_func.R"))
source(here::here("Data", "Preparing data" , "Time invariant", "cluster_migration_func_2020_outRate.R"))
source(here::here("Data", "Preparing data" , "Time invariant", "fertility_matrix_func.R"))
source(here::here("Data", "Preparing data" , "Time invariant", "migration_matrix_func.R"))
source(here::here("Data", "Preparing data" , "Time invariant", "migration_matrix_func_MS.R"))
source(here::here("Data", "Preparing data" , "Time invariant", "mortality_matrix_func.R"))

## In case we want to save some pictures....
fig_out <- here::here("Data", "Figures of rates" , "Invariant")
fs::dir_create(fig_out)


## read in mortality and fertility over the periods (1981-2021)
deaths_data <- readxl::read_xlsx( here::here(  "Data" , "Demogaphic data" , "LAD_deaths_1981_2021.xlsx"), skip=5, sheet=4)
fert_data <- readr::read_csv(here::here("Data", "Demogaphic data" , "ew_asfrs.csv"))

#### read in the exposures (population sizes) over period (1991-2021)
list_of_years_needed5 <- c("1991" , "1992" ,"1993" ,"1994" )
list_of_years_needed4 <- c("1995" ,"1996" ,"1997" , "1998" , "1999" ,"2000" , "2001" )
list_of_years_needed3 <- c("2002" ,"2003" ,"2004" , "2005" , "2006" ,"2007" , "2008" )
list_of_years_needed2 <- c("2009" ,"2010" ,"2011" , "2012" , "2013" ,"2014" , "2015" )
list_of_years_needed1 <- c("2016" , "2017" ,"2018" , "2019" , "2020" , "2021")

## make a data frame of exposures for age class over LAD, by year
exposures_data <- time_series_exposures(list(list_of_years_needed1,list_of_years_needed2,list_of_years_needed3,
                                             list_of_years_needed4,list_of_years_needed5))

##### Read in migration data. Data comes in 2 parts, which I rbind -- e.g.,

l18 <- readr::read_csv( here::here( "Data",  "Demogaphic data" , "2020_p1.csv" ))
l19 <- readr::read_csv( here::here(  "Data" , "Demogaphic data" , "2020_p2.csv" ))

### Cluster the age-specific migration between LADS, choice here is by "outRate" or "inRate"  
## Inputs: choose number of stages (clusters to use) -- here no_stage = 7
CLUST_MIG_2020 <- cluster_migration_timeSeries_outRate2020(list(l18,l19) , no_stage)

### Data frame of migration counts and cluster for each age across each LAD
clustered_migration_df <- CLUST_MIG_2020[[1]]
lad_ts_df <- CLUST_MIG_2020[[2]]

### Obtain death rates by combining exposures with death counts
AA <- align_mort_exposures(deaths_data, exposures_data)
deaths_counts_df <- AA[[1]]
exposures_mig <- AA[[2]]

## Match the migration-clustered LADS to death and fertility rates
aligned_clustered_rates <- take_cluster_mig_align_fert(fert_data, clustered_migration_df, deaths_counts_df)

######################### Construct our survival martices, U_{no_stages}
#                                                          F_{no_ages}
#                                                          H_{no_stages}
#                                                          T_{no_ages}


U_list <- create_mort_mats1(clustered_migration_df, aligned_clustered_rates[[2]])
## check 
U_list[[1]]

F_list <- create_fert_mats(clustered_migration_df , aligned_clustered_rates[[1]])
## check
F_list[[4]]

#### T_list_1YEAR gives the stochastic transfer matrices for 1 year migration
T_list_1YEAR <- create_mig_mats(clustered_migration_df, lad_ts_df, exposures_mig)
### We put the output data T_list_1YEAR[[2]] into the file "interp_mig_K7.R" to interpolate to 1 year age classes
### The result is a 1 year age migration list "one_year_T_list" which we run in parallel to the 5 year list

CHECK_DATA <- T_list_1YEAR[[2]]

CHECK_DATA%>%head()

CHECK_DATA$Age_start <- gsub('_.*','',CHECK_DATA$Age)

CHECK_DATA$Age_start <- as.numeric(CHECK_DATA$Age_start)
CHECK_DATA$Age_start%>%unique()%>%length()

############################################### K1 going out ##############################

## k1 -> k2
k1_to_k2_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k1_to_k2_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 2")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 2")%>%
    dplyr::select(exposures)%>%sum() }
k1_to_k2_list[[19]] <- 0
k1_to_k2_list

## k1 -> k3
k1_to_k3_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k1_to_k3_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 3")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 3")%>%
    dplyr::select(exposures)%>%sum() }
k1_to_k3_list[[19]] <- 0
k1_to_k3_list

## k1 -> k4
k1_to_k4_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k1_to_k4_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 4")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 4")%>%
    dplyr::select(exposures)%>%sum() }
k1_to_k4_list[[19]] <- 0
k1_to_k4_list

## k1 -> k5
k1_to_k5_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k1_to_k5_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 5")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 5")%>%
    dplyr::select(exposures)%>%sum() }
k1_to_k5_list[[19]] <- 0
k1_to_k5_list

## k1 -> k6
k1_to_k6_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k1_to_k6_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 6")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 6")%>%
    dplyr::select(exposures)%>%sum() }
k1_to_k6_list[[19]] <- 0
k1_to_k6_list

## k1 -> k7
k1_to_k7_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k1_to_k7_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 7")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 1" , dest_cluster == "K = 7")%>%
    dplyr::select(exposures)%>%sum() }
k1_to_k7_list[[19]] <- 0
k1_to_k7_list


k1_to_k1_list <- 1 - k1_to_k2_list - k1_to_k3_list - k1_to_k4_list - k1_to_k5_list - k1_to_k6_list - k1_to_k7_list


k1k2_int <- approx(seq(0,95,5) , c(k1_to_k2_list,0), seq(0,95,1))$y
k1k3_int <- approx(seq(0,95,5) , c(k1_to_k3_list,0), seq(0,95,1))$y
k1k4_int <- approx(seq(0,95,5) , c(k1_to_k4_list,0), seq(0,95,1))$y
k1k5_int <- approx(seq(0,95,5) , c(k1_to_k5_list,0), seq(0,95,1))$y
k1k6_int <- approx(seq(0,95,5) , c(k1_to_k6_list,0), seq(0,95,1))$y
k1k7_int <- approx(seq(0,95,5) , c(k1_to_k7_list,0), seq(0,95,1))$y
k1k1_int <- approx(seq(0,95,5) , c(k1_to_k1_list,1), seq(0,95,1))$y

plot(k1k1_int)
############################################### K2 going out ##############################

## k2 -> k1
k2_to_k1_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k2_to_k1_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 1")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 1")%>%
    dplyr::select(exposures)%>%sum() }
k2_to_k1_list[[19]] <- 0
k2_to_k1_list

## k2 -> k3
k2_to_k3_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k2_to_k3_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 3")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 3")%>%
    dplyr::select(exposures)%>%sum() }
k2_to_k3_list[[19]] <- 0
k2_to_k3_list

## k2 -> k4
k2_to_k4_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k2_to_k4_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 4")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 4")%>%
    dplyr::select(exposures)%>%sum() }
k2_to_k4_list[[19]] <- 0
k2_to_k4_list

## k2 -> k5
k2_to_k5_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k2_to_k5_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 5")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 5")%>%
    dplyr::select(exposures)%>%sum() }
k2_to_k5_list[[19]] <- 0
k2_to_k5_list

## k2 -> k6
k2_to_k6_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k2_to_k6_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 6")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 6")%>%
    dplyr::select(exposures)%>%sum() }
k2_to_k6_list[[19]] <- 0
k2_to_k6_list

## k2 -> k7
k2_to_k7_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k2_to_k7_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 7")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 2" , dest_cluster == "K = 7")%>%
    dplyr::select(exposures)%>%sum() }
k2_to_k7_list[[19]] <- 0
k2_to_k7_list




k2_to_k2_list <- 1 - k2_to_k1_list - k2_to_k3_list - k2_to_k4_list - k2_to_k5_list - k2_to_k6_list - k2_to_k7_list

k2k1_int <- approx(seq(0,95,5) , c(k2_to_k1_list,0), seq(0,95,1))$y
k2k3_int <- approx(seq(0,95,5) , c(k2_to_k3_list,0), seq(0,95,1))$y
k2k4_int <- approx(seq(0,95,5) , c(k2_to_k4_list,0), seq(0,95,1))$y
k2k5_int <- approx(seq(0,95,5) , c(k2_to_k5_list,0), seq(0,95,1))$y
k2k6_int <- approx(seq(0,95,5) , c(k2_to_k6_list,0), seq(0,95,1))$y
k2k7_int <- approx(seq(0,95,5) , c(k2_to_k7_list,0), seq(0,95,1))$y
k2k2_int <- approx(seq(0,95,5) , c(k2_to_k2_list,1), seq(0,95,1))$y

plot(k2k2_int)
plot(k2_to_k2_list)
######################################### K3 out ##############################################################

## k3 -> k1
k3_to_k1_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k3_to_k1_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 1")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 1")%>%
    dplyr::select(exposures)%>%sum() }
k3_to_k1_list[[19]] <- 0
k3_to_k1_list

## k3 -> k2
k3_to_k2_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k3_to_k2_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 2")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 2")%>%
    dplyr::select(exposures)%>%sum() }
k3_to_k2_list[[19]] <- 0
k3_to_k2_list

## k3 -> k4
k3_to_k4_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k3_to_k4_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 4")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 4")%>%
    dplyr::select(exposures)%>%sum() }
k3_to_k4_list[[19]] <- 0
k3_to_k4_list

## k2 -> k5
k3_to_k5_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k3_to_k5_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 5")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 5")%>%
    dplyr::select(exposures)%>%sum() }
k3_to_k5_list[[19]] <- 0
k3_to_k5_list

## k3 -> k6
k3_to_k6_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k3_to_k6_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 6")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 6")%>%
    dplyr::select(exposures)%>%sum() }
k3_to_k6_list[[19]] <- 0
k3_to_k6_list

## k3 -> k7
k3_to_k7_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k3_to_k7_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 7")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 3" , dest_cluster == "K = 7")%>%
    dplyr::select(exposures)%>%sum() }
k3_to_k7_list[[19]] <- 0
k3_to_k7_list


k3_to_k3_list <- 1 - k3_to_k1_list - k3_to_k2_list - k3_to_k4_list - k3_to_k5_list - k3_to_k6_list - k3_to_k7_list

k3k1_int <- approx(seq(0,95,5) , c(k3_to_k1_list,0), seq(0,95,1))$y
k3k3_int <- approx(seq(0,95,5) , c(k3_to_k3_list,1), seq(0,95,1))$y
k3k4_int <- approx(seq(0,95,5) , c(k3_to_k4_list,0), seq(0,95,1))$y
k3k5_int <- approx(seq(0,95,5) , c(k3_to_k5_list,0), seq(0,95,1))$y
k3k6_int <- approx(seq(0,95,5) , c(k3_to_k6_list,0), seq(0,95,1))$y
k3k7_int <- approx(seq(0,95,5) , c(k3_to_k7_list,0), seq(0,95,1))$y
k3k2_int <- approx(seq(0,95,5) , c(k3_to_k2_list,0), seq(0,95,1))$y

plot(k3k3_int)
plot(k3_to_k3_list)

######################################### k4 going out #####################################


## k4 -> k1
k4_to_k1_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k4_to_k1_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 1")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 1")%>%
    dplyr::select(exposures)%>%sum() }
k4_to_k1_list[[19]] <- 0
k4_to_k1_list

## k4 -> k2
k4_to_k2_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k4_to_k2_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 2")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 2")%>%
    dplyr::select(exposures)%>%sum() }
k4_to_k2_list[[19]] <- 0
k4_to_k2_list

## k4 -> k3
k4_to_k3_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k4_to_k3_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 3")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 3")%>%
    dplyr::select(exposures)%>%sum() }
k4_to_k3_list[[19]] <- 0
k4_to_k3_list

## k4 -> k5
k4_to_k5_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k4_to_k5_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 5")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 5")%>%
    dplyr::select(exposures)%>%sum() }
k4_to_k5_list[[19]] <- 0
k4_to_k5_list

## k4 -> k6
k4_to_k6_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k4_to_k6_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 6")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 6")%>%
    dplyr::select(exposures)%>%sum() }
k4_to_k6_list[[19]] <- 0
k4_to_k6_list

## k2 -> k7
k4_to_k7_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k4_to_k7_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 7")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 4" , dest_cluster == "K = 7")%>%
    dplyr::select(exposures)%>%sum() }
k4_to_k7_list[[19]] <- 0
k4_to_k7_list


k4_to_k4_list <- 1 - k4_to_k1_list - k4_to_k2_list - k4_to_k3_list - k4_to_k5_list - k4_to_k6_list - k4_to_k7_list

k4k1_int <- approx(seq(0,95,5) , c(k4_to_k1_list,0), seq(0,95,1))$y
k4k2_int <- approx(seq(0,95,5) , c(k4_to_k2_list,0), seq(0,95,1))$y
k4k3_int <- approx(seq(0,95,5) , c(k4_to_k3_list,0), seq(0,95,1))$y
k4k5_int <- approx(seq(0,95,5) , c(k4_to_k5_list,0), seq(0,95,1))$y
k4k6_int <- approx(seq(0,95,5) , c(k4_to_k6_list,0), seq(0,95,1))$y
k4k7_int <- approx(seq(0,95,5) , c(k4_to_k7_list,0), seq(0,95,1))$y
k4k4_int <- approx(seq(0,95,5) , c(k4_to_k4_list,1), seq(0,95,1))$y


############################# k5 going out ##########################################################################


## k5 -> k1
k5_to_k1_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k5_to_k1_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 1")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 1")%>%
    dplyr::select(exposures)%>%sum() }
k5_to_k1_list[[19]] <- 0
k5_to_k1_list

## k4 -> k2
k5_to_k2_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k5_to_k2_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 2")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 2")%>%
    dplyr::select(exposures)%>%sum() }
k5_to_k2_list[[19]] <- 0
k5_to_k2_list

## k4 -> k3
k5_to_k3_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k5_to_k3_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 3")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 3")%>%
    dplyr::select(exposures)%>%sum() }
k5_to_k3_list[[19]] <- 0
k5_to_k3_list

## k4 -> k5
k5_to_k4_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k5_to_k4_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 4")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 4")%>%
    dplyr::select(exposures)%>%sum() }
k5_to_k4_list[[19]] <- 0
k5_to_k4_list

## k4 -> k6
k5_to_k6_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k5_to_k6_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 6")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 6")%>%
    dplyr::select(exposures)%>%sum() }
k5_to_k6_list[[19]] <- 0
k5_to_k6_list

## k5 -> k7
k5_to_k7_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k5_to_k7_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 7")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 5" , dest_cluster == "K = 7")%>%
    dplyr::select(exposures)%>%sum() }
k5_to_k7_list[[19]] <- 0
k5_to_k7_list


k5_to_k5_list <- 1 - k5_to_k1_list - k5_to_k2_list - k5_to_k3_list - k5_to_k4_list - k5_to_k6_list - k5_to_k7_list

k5k1_int <- approx(seq(0,95,5) , c(k5_to_k1_list,0), seq(0,95,1))$y
k5k2_int <- approx(seq(0,95,5) , c(k5_to_k2_list,0), seq(0,95,1))$y
k5k3_int <- approx(seq(0,95,5) , c(k5_to_k3_list,0), seq(0,95,1))$y
k5k5_int <- approx(seq(0,95,5) , c(k5_to_k5_list,1), seq(0,95,1))$y
k5k6_int <- approx(seq(0,95,5) , c(k5_to_k6_list,0), seq(0,95,1))$y
k5k7_int <- approx(seq(0,95,5) , c(k5_to_k7_list,0), seq(0,95,1))$y
k5k4_int <- approx(seq(0,95,5) , c(k5_to_k4_list,0), seq(0,95,1))$y


############################################## k6 going out #######################################



## k5 -> k1
k6_to_k1_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k6_to_k1_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 1")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 1")%>%
    dplyr::select(exposures)%>%sum() }
k6_to_k1_list[[19]] <- 0
k6_to_k1_list

## k4 -> k2
k6_to_k2_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k6_to_k2_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 2")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 2")%>%
    dplyr::select(exposures)%>%sum() }
k6_to_k2_list[[19]] <- 0
k6_to_k2_list

## k4 -> k3
k6_to_k3_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k6_to_k3_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 3")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 3")%>%
    dplyr::select(exposures)%>%sum() }
k6_to_k3_list[[19]] <- 0
k6_to_k3_list

## k4 -> k5
k6_to_k4_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k6_to_k4_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 4")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 4")%>%
    dplyr::select(exposures)%>%sum() }
k6_to_k4_list[[19]] <- 0
k6_to_k4_list

## k4 -> k6
k6_to_k5_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k6_to_k5_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 5")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 5")%>%
    dplyr::select(exposures)%>%sum() }
k6_to_k5_list[[19]] <- 0
k6_to_k5_list

## k5 -> k7
k6_to_k7_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k6_to_k7_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 7")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 6" , dest_cluster == "K = 7")%>%
    dplyr::select(exposures)%>%sum() }
k6_to_k7_list[[19]] <- 0
k6_to_k7_list


k6_to_k6_list <- 1 - k6_to_k1_list - k6_to_k2_list - k6_to_k3_list - k6_to_k4_list - k6_to_k5_list - k6_to_k7_list

k6k1_int <- approx(seq(0,95,5) , c(k6_to_k1_list,0), seq(0,95,1))$y
k6k2_int <- approx(seq(0,95,5) , c(k6_to_k2_list,0), seq(0,95,1))$y
k6k3_int <- approx(seq(0,95,5) , c(k6_to_k3_list,0), seq(0,95,1))$y
k6k5_int <- approx(seq(0,95,5) , c(k6_to_k5_list,0), seq(0,95,1))$y
k6k6_int <- approx(seq(0,95,5) , c(k6_to_k6_list,1), seq(0,95,1))$y
k6k7_int <- approx(seq(0,95,5) , c(k6_to_k7_list,0), seq(0,95,1))$y
k6k4_int <- approx(seq(0,95,5) , c(k6_to_k4_list,0), seq(0,95,1))$y



############### k7 going out


## k7 -> k1
k7_to_k1_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k7_to_k1_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 1")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 1")%>%
    dplyr::select(exposures)%>%sum() }
k7_to_k1_list[[19]] <- 0
k7_to_k1_list

## k7 -> k2
k7_to_k2_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k7_to_k2_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 2")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 2")%>%
    dplyr::select(exposures)%>%sum() }
k7_to_k2_list[[19]] <- 0
k7_to_k2_list

## k4 -> k3
k7_to_k3_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k7_to_k3_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 3")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 3")%>%
    dplyr::select(exposures)%>%sum() }
k7_to_k3_list[[19]] <- 0
k7_to_k3_list

## k4 -> k5
k7_to_k4_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k7_to_k4_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 4")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 4")%>%
    dplyr::select(exposures)%>%sum() }
k7_to_k4_list[[19]] <- 0
k7_to_k4_list

## k4 -> k6
k7_to_k5_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k7_to_k5_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 5")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 5")%>%
    dplyr::select(exposures)%>%sum() }
k7_to_k5_list[[19]] <- 0
k7_to_k5_list

## k7 -> k6
k7_to_k6_list <- rep(NA, 19)
for( i in seq(0, 90, 5)){
  k7_to_k6_list[[i/5+1]] <- CHECK_DATA%>%
    filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 6")%>%
    dplyr::select(moves)%>%sum() / CHECK_DATA%>%filter(Age_start == i, orig_cluster == "K = 7" , dest_cluster == "K = 6")%>%
    dplyr::select(exposures)%>%sum() }
k7_to_k6_list[[19]] <- 0
k7_to_k6_list


k7_to_k7_list <- 1 - k7_to_k1_list - k7_to_k2_list - k7_to_k3_list - k7_to_k4_list - k7_to_k5_list - k7_to_k6_list

k7k1_int <- approx(seq(0,95,5) , c(k7_to_k1_list,0), seq(0,95,1))$y
k7k2_int <- approx(seq(0,95,5) , c(k7_to_k2_list,0), seq(0,95,1))$y
k7k3_int <- approx(seq(0,95,5) , c(k7_to_k3_list,0), seq(0,95,1))$y
k7k5_int <- approx(seq(0,95,5) , c(k7_to_k5_list,0), seq(0,95,1))$y
k7k6_int <- approx(seq(0,95,5) , c(k7_to_k6_list,0), seq(0,95,1))$y
k7k7_int <- approx(seq(0,95,5) , c(k7_to_k7_list,1), seq(0,95,1))$y
k7k4_int <- approx(seq(0,95,5) , c(k7_to_k4_list,0), seq(0,95,1))$y


#########################################

############################################################################################

######################################################################################################################################


one_year_T_list <- list()
for(i in 1:(length(k1k1_int))){
  
  tran_m <- matrix(NA, 7, 7)
  
  tran_m[1,1] <- k1k1_int[[i]]
  tran_m[2,1] <- k2k1_int[[i]]
  tran_m[3,1] <- k3k1_int[[i]]
  tran_m[4,1] <- k4k1_int[[i]]
  tran_m[5,1] <- k5k1_int[[i]]
  tran_m[6,1] <- k6k1_int[[i]]
  tran_m[7,1] <- k7k1_int[[i]]
  
  tran_m[1,2] <- k1k2_int[[i]]
  tran_m[2,2] <- k2k2_int[[i]]
  tran_m[3,2] <- k3k2_int[[i]]
  tran_m[4,2] <- k4k2_int[[i]]
  tran_m[5,2] <- k5k2_int[[i]]
  tran_m[6,2] <- k6k2_int[[i]]
  tran_m[7,2] <- k7k2_int[[i]]
  
  tran_m[1,3] <- k1k3_int[[i]]
  tran_m[2,3] <- k2k3_int[[i]]
  tran_m[3,3] <- k3k3_int[[i]]
  tran_m[4,3] <- k4k3_int[[i]]
  tran_m[5,3] <- k5k3_int[[i]]
  tran_m[6,3] <- k6k3_int[[i]]
  tran_m[7,3] <- k7k3_int[[i]]
  
  tran_m[1,4] <- k1k4_int[[i]]
  tran_m[2,4] <- k2k4_int[[i]]
  tran_m[3,4] <- k3k4_int[[i]]
  tran_m[4,4] <- k4k4_int[[i]]
  tran_m[5,4] <- k5k4_int[[i]]
  tran_m[6,4] <- k6k4_int[[i]]
  tran_m[7,4] <- k7k4_int[[i]]
  
  #
  tran_m[1,5] <- k1k5_int[[i]]
  tran_m[2,5] <- k2k5_int[[i]]
  tran_m[3,5] <- k3k5_int[[i]]
  tran_m[4,5] <- k4k5_int[[i]]
  tran_m[5,5] <- k5k5_int[[i]]
  tran_m[6,5] <- k6k5_int[[i]]
  tran_m[7,5] <- k7k5_int[[i]]
  
  tran_m[1,6] <- k1k6_int[[i]]
  tran_m[2,6] <- k2k6_int[[i]]
  tran_m[3,6] <- k3k6_int[[i]]
  tran_m[4,6] <- k4k6_int[[i]]
  tran_m[5,6] <- k5k6_int[[i]]
  tran_m[6,6] <- k6k6_int[[i]]
  tran_m[7,6] <- k7k6_int[[i]]
  
  tran_m[1,7] <- k1k7_int[[i]]
  tran_m[2,7] <- k2k7_int[[i]]
  tran_m[3,7] <- k3k7_int[[i]]
  tran_m[4,7] <- k4k7_int[[i]]
  tran_m[5,7] <- k5k7_int[[i]]
  tran_m[6,7] <- k6k7_int[[i]]
  tran_m[7,7] <- k7k7_int[[i]]
  
  one_year_T_list[[i]] <- tran_m
  
}



T_list1 <- create_mig_mats_MS(clustered_migration_df, lad_ts_df, exposures_mig, one_year_T_list)
## Now put T_list1's outputs I): a 5 year probability of moving and II) a 1 year probability of moving
## into the file "Migration_Mover_Stayer_Model.R" to obtain the semi-Markov approximate 5 year probabilities

## list of age-specific final implied 5 year migration probabilities (19 long)
age_spec_mat_list_new <- list()

## list of matrices to compare with one year rates
age_spec_mat_list_1yr_checks <- list()

### list of data to power 1
one_year_migrations <- T_list1[[1]]
### list of data to power 5
five_year_migrations <- T_list1[[2]]

one_year_migrations[[12]]
one_year_migrations
five_year_migrations

for(age in 1:19){
  
  P10_1 <- t(one_year_migrations[[(age)]])
  P10_5 <- t(five_year_migrations[[(age)]])
  
  T10_1 <- eigen(P10_1)$vectors
  T10_5 <- eigen(P10_5)$vectors
  
  
  diag_T10_1 <- matrix(0, 7, 7)
  diag_T10_5 <- matrix(0, 7, 7)
  diag(diag_T10_1) <- eigen(P10_1)$values
  diag(diag_T10_5) <- eigen(P10_5)$values
  
  d10_1 <- diag_T10_1[2:7,2:7]
  d10_5 <- diag_T10_5[2:7,2:7]
  
  
  sopt <- nlminb( c(1, 1, 1, 1, 1, 1, 0.05, 0.01), function(X){
    m1 <- diag(d10_1)
    m2 <- diag(d10_5)
    sum_to_minimise <- 0
    foreach(i = 1:6)%do%{
      sum_to_minimise <- sum_to_minimise + 
        (m1[i] - X[7]*X[i] - (1-X[7])*X[i]^(X[8]) )^2 + 
        (m2[i] - X[7]*X[i]^5 - (1-X[7])*X[i]^(5*X[8]))^2
    }
    return(sum_to_minimise)} , 
    gradient = NULL , 
    hessian = NULL , 
    lower = c(0, 0 , 0, 0, 0, 0, 0.01, 0.01) , 
    upper = c(1, 1, 1, 1, 1, 1, 1, 1))
  
  
  
  
  vec1 <- log(sopt$par[1])
  vec2 <- log(sopt$par[2])
  vec3 <- log(sopt$par[3])
  vec4 <- log(sopt$par[4])
  vec5 <- log(sopt$par[5])
  vec6 <- log(sopt$par[6])
  a1 <- sopt$par[7]
  k1 <- sopt$par[8]
  
  
  diag_pi_1 <- matrix(0, 7, 7)
  diag(diag_pi_1) <- Re(c(1, exp(vec1), exp(vec2), exp(vec3), exp(vec4), exp(vec5), exp(vec6) ))
  diag_rho_1 <- matrix(0, 7, 7)
  diag(diag_rho_1) <- Re(c(1, exp(k1*vec1), exp(k1*vec2), exp(k1*vec3) ,exp(k1*vec4) ,exp(k1*vec5) ,exp(k1*vec6) ))
  
  PI_10_1 <- Re(T10_1%*%diag_pi_1%*%ginv(T10_1))
  RHO_10_1 <- Re(T10_1%*%diag_rho_1%*%ginv(T10_1))
  
  diag_pi_5 <- matrix(0, 7, 7)
  diag(diag_pi_5) <- Re(c(1, exp(5*vec1), exp(5*vec2), exp(5*vec3), exp(5*vec4), exp(5*vec5), exp(5*vec6)))
  diag_rho_5 <- matrix(0, 7, 7)
  diag(diag_rho_5) <- Re(c(1, exp(5*k1*vec1), exp(5*k1*vec2), exp(5*k1*vec3), exp(5*k1*vec4), exp(5*k1*vec5), exp(5*k1*vec6)))
  
  PI_10_5 <- Re(T10_1%*%diag_pi_5%*%ginv(T10_1))
  RHO_10_5 <- Re(T10_1%*%diag_rho_5%*%ginv(T10_1))
  
  ### resulting matrices
  
  age_spec_mat_list_1yr_checks[[age]] <- a1*PI_10_1 + (1-a1)*RHO_10_1
  age_spec_mat_list_new[[ age ]] <-   a1*PI_10_5 + (1-a1)*RHO_10_5
  age_spec_mat_list_new[[age]] <- t(age_spec_mat_list_new[[age]])
  
  
}

T_list <- age_spec_mat_list_new


# H_list is the re-distribution 
no_age = nrow(U_list[[1]])
H <- matrix(0, no_age, no_age)
H[1,] <- rep(1, no_age)
H_list <- list()
for(stage in 1:no_stage){
  H_list[[(1+length(H_list))]] <- H
}
H_list



#### Check matrix lists and dimensions

U_list%>%length()
U_list[[1]]%>%nrow()
F_list%>%length()
F_list[[1]]%>%nrow()
H_list%>%length()
H_list[[1]]%>%nrow()
T_list%>%length()
T_list[[1]]%>%nrow()


### Visualisation of the demographic rates.
discrete_colours <- RColorBrewer::brewer.pal(10, "Set1")

## Mortality and fertility profiles are outputs of the function: "take_cluster_mig_align_fert"
### Log death rate for all clusters
lg_plt <- aligned_clustered_rates[[3]]
### Log death rate cluster averge
lg_plt <- aligned_clustered_rates[[2]]%>%mutate(rate = deaths/counts)%>%group_by(Age,cluster)%>%
  summarise(dr = mean(rate))%>%
  mutate(num_age = gsub('.{2}$' , "" , Age),
         num_age = as.numeric(gsub('_' , "" , num_age)),
         Age = gsub('_' , "-" , Age),
         Age = fct_reorder(Age, num_age))%>%
  filter(dr>-100)%>%
  na.omit()%>%
  ggplot(aes(x = Age, y = log(dr) , color = cluster))   +
  geom_line(aes(group = as.factor(cluster))) + 
  theme(legend.position = "top") + theme_bw()  + 
  scale_color_manual(labels = seq(1,no_stage,1),
                     values = discrete_colours[1 : no_stage])+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        text = element_text(size = 14)) + 
  ggeasy::easy_center_title() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                                      text = element_text(size = 14),
                                      legend.position = "top") + ylab("Log death rate") + xlab("Age class")


lg_plt

ggsave( paste0( fig_out ,"/", "death_rate_outR.png") , lg_plt, width = 8, height = 6, dpi = 300)


### Fertility rate for each cluster
fert_plt <- aligned_clustered_rates[[5]]
### Fertility averages by cluster
fert_plt <-aligned_clustered_rates[[1]]%>%group_by(Age,cluster1)%>%summarise(asfr = mean(ASFR))%>%
  mutate(num_age = gsub('.{2}$' , "" , Age),
         num_age = as.numeric(gsub('_' , "" , num_age)),
         Age = gsub('_' , "-" , Age),
         Age = fct_reorder(Age, num_age),
         cluster = cluster1)%>%
  ggplot(aes(x = Age, y = asfr, color = cluster)) + 
  geom_line(aes(group = factor(cluster)))  +
  scale_color_manual(labels = seq(1, no_stage, 1),
                     values = discrete_colours[1 : no_stage]) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     text = element_text(size = 14)) + 
  ggeasy::easy_center_title() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                                      text = element_text(size = 14),
                                      legend.position = "top") + ylab("ASFR") + xlab("Age class")


ggsave( paste0( fig_out ,"/", "fert_rate_outR.png") , fert_plt, width = 8, height = 6, dpi = 300)

## Migration rates are visualised from output of "cluster_mig_func"
mig_plot <- CLUST_MIG_2020[[4]] ## time averaged
ggsave( paste0( fig_out ,"/", "mig_rate_outR.png") , mig_plot, width = 8, height = 6, dpi = 300)
mig_plot
CLUST_MIG_2020[[3]] ## year by year



################################### UK mapping #######################################

library(sf)
map <- st_read(here::here(  "Data" , , "Demographic data", "LAD_DEC_2021_UK_BUC.shp"))
shp <- fortify(map, region = 'LAD21CD')
shp$index <- 1:nrow(shp)
link <- read.csv(here::here(  "Data"  ,  "Demographic data", "Local_Authority_Districts_December_2021_UK_BUC_2022.csv"))
names(link)[1] <- "index"
names(link)[2] <- "LA21CD"
shp <- merge(shp, link, by.x = 'index', by.y = 'index', all.x = TRUE) %>% rename("LAD"=LAD21NM.x,"Code"=LAD21CD)
shp %<>% filter(substr(Code,1,1) != "N")
shp$LAD[which(shp$LAD=="Rhondda Cynon Taf")] <- "Rhondda Cynon Taff"
shp%>%head()
clustered_migration_df
tmpc2 <- left_join(clustered_migration_df %>%dplyr::select(LAD_code,cluster)%>%
                     mutate(Code = LAD_code), shp)
plot_MAP <- tmpc2 %>%
  ggplot() +
  geom_sf(aes(fill=factor(cluster),geometry=geometry),linewidth=0.05,color="black") +
  scale_fill_manual(values=discrete_colours[1:7]) +
  theme_void() +
  labs(fill="Cluster")
ggsave( paste0( fig_out ,"/", "UK_clusters.png") , plot_MAP)

clustered_migration_df%>%filter(cluster==1)%>%dplyr::select(LAD_code)%>%unique()%>%nrow()
clustered_migration_df$LAD_code%>%unique()%>%length()
shp$Code%>%length()


clustered_migration_df$Age_class%>%unique()

CHECK_DATA <- T_list[[2]]

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

tran_m
apply(one_year_T_list[[18]] , 1, sum)

one_year_T_list[[1]] %*% one_year_T_list[[2]] %*% one_year_T_list[[3]] %*% one_year_T_list[[4]] %*% one_year_T_list[[5]]
length(one_year_T_list)
length(k1k1_int)

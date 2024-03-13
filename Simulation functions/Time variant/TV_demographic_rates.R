source(here::here("Matrix model"  , "General functions" , "matrix_operations.R"))

U_TV <- read_rds(here::here("Data", "Time variant", "U_list_by_year.Rds"))
F_TV <- read_rds(here::here("Data", "Time variant", "F_list_by_year.Rds"))
TT <- read_rds(here::here("Data", "Time invariant", "T_list.Rds"))
TT <- lapply(TT, function(x){abs(x)})
HH <- read_rds(here::here("Data", "Time invariant", "H_list.Rds"))
no_stage <- length(UU)
no_age <- nrow(UU[[1]])

for(j in 11:18){
  F_TV[[j]] <- F_TV[[11]]}

length(F_TV)
length(U_TV)

Fers <- list()
for(j in 1:length(F_TV)){
  FF <- F_TV[[j]]
  f_1 <- list()
  f_2 <- list()
  f_3 <- list()
  f_4 <- list()
  f_5 <- list()
  f_6 <- list()
  f_7 <- list()
  for(i in 1: length(FF)){
    f_1[[(1+length(f_1))]] <- diag(FF[[i]])[1]
    f_2[[(1+length(f_2))]] <- diag(FF[[i]])[2]
    f_3[[(1+length(f_3))]] <- diag(FF[[i]])[3]
    f_4[[(1+length(f_4))]] <- diag(FF[[i]])[4]
    f_5[[(1+length(f_5))]] <- diag(FF[[i]])[5]
    f_6[[(1+length(f_6))]] <- diag(FF[[i]])[6]
    f_7[[(1+length(f_7))]] <- diag(FF[[i]])[7]
  }
  f_1 <- unlist(f_1)
  f_2 <- unlist(f_2)
  f_3 <- unlist(f_3)
  f_4 <- unlist(f_4)
  f_5 <- unlist(f_5)
  f_6 <- unlist(f_6)
  f_7 <- unlist(f_7)
  
  Fers[[j]] <- list(f_1,f_2,f_3,f_4,f_5,f_6,f_7)}

Fertility_list <- Fers

################################
Mots<-list()
for(j in 1:18){
  UU <- U_TV[[j]]
  u_1 <- c(diag(UU[[1]][-1,-ncol(UU[[1]])]),0)
  u_2 <- c(diag(UU[[2]][-1,-ncol(UU[[2]])]),0)
  u_3 <- c(diag(UU[[3]][-1,-ncol(UU[[3]])]),0)
  u_4 <- c(diag(UU[[4]][-1,-ncol(UU[[4]])]),0)
  u_5 <- c(diag(UU[[5]][-1,-ncol(UU[[5]])]),0)
  u_6 <- c(diag(UU[[6]][-1,-ncol(UU[[6]])]),0)
  u_7 <- c(diag(UU[[7]][-1,-ncol(UU[[7]])]),0)
  Mots[[j]] <- list(u_1,u_2,u_3,u_4,u_5,u_6,u_7)
}
Mots%>%length()

Mortality_list <- Mots

UB <- block_diag_function(U_TV[[1]]) ## BD age survival matrix (one block each stage)
FB <- block_diag_function(F_TV[[1]]) ## BD Fertility between stage matrix (one block for each age)
TB <- block_diag_function(TT) ## BD stage transition matrix (one block each stage)
HB <- block_diag_function(HH) ## BD matrix assigning newborns to a given age (one for each stage)
# Check dimensions
dim(UB)
dim(FB)
dim(TB)
dim(HB)
## Projection matrices
no_age <- 19
no_stage <- 7
U_proj <- t(K_perm_mat(no_stage, no_age))%*%UB%*%K_perm_mat(no_stage, no_age)%*%TB
F_proj <- t(K_perm_mat(no_stage, no_age))%*%HB%*%K_perm_mat(no_stage, no_age)%*%FB

sps <- SD(U_proj + F_proj)
mad<-sps[seq(1,19*7,7)]
msd<-sps[seq(1,19*7,19)]

hist(mad)
df<-data.frame(age = rep(NA,7*19),stage = rep(NA,7*19), value = rep(NA,7*19))
for(i in 1:length(sps)){
  index_age <- (i-1)%/%7
  index_stage <- (i-1)%%7 + 1
  df$age[i] <- index_age
  df$stage[i] <- index_stage
  df$value[i] <- sps[i]*10000
  
}

df%>%summarise(sum(value))
df%>%group_by(stage)%>%summarise(sum = sum(value)/10000)
tot_age_class <- df%>%group_by(age)%>%summarise(sum = sum(value))%>%as.data.frame()
tot_age_class <- round(tot_age_class[,2])
tot_age_class

age_dist <- rep(df$age, times = round(df$value) )
age_dist
stage_dist <- rep(df$stage, times= round(df$value))
stage_dist
hist(age_dist)
hist(stage_dist)
stage_dist
age_dist

source(here::here("Simulation functions","Demographic events","have_sex.R"))
source(here::here("Simulation functions","Demographic events","change_stage.R"))
source(here::here("Simulation functions","Demographic events","kill_individuals.R"))
source(here::here("Simulation functions","Demographic events","get_older.R"))

########### SIM YS #####################################

no_reps <- 5000
sim_list <- list()
foreach(reps = 1:no_reps)%do%{
  
  Mots_IC <- Mortality_list[[1]]
  Fers_IC <- Fertility_list[[1]]
  
  ### Initialise population
  ind <-  data.frame(ID = seq(1,length(age_dist),1), 
                     Age = rep(age_dist, 1), ## marginal stage dist
                     stage = as.numeric(rep(stage_dist, 1)), ## marginal stage dist
                     mother_ID = rep(NA, length(stage_dist)), ## mother ID links Focal and other kin
                     alive = rep(1,length(age_dist)),
                     mig_probs = NA) 
  
  tran_prob <- lapply(1:nrow(ind), function(x){
    age <- ind$Age[x] + 1
    stage <- ind$stage[x]
    return(TT[[age]][,stage])})
  ind$mig_probs <- tran_prob
  
  babies_prob <- unlist(lapply(1:nrow(ind), function(x){
    age <- ind$Age[x] + 1
    stage <- ind$stage[x]
    return(Fers_IC[[stage]][age])}) )
  ind$prob_have_sex_and_child <- babies_prob
  
  die_prob <- unlist(lapply(1:nrow(ind), function(x){
    age <- ind$Age[x] + 1
    stage <- ind$stage[x]
    return(1- Mots_IC[[stage]][age])}) )
  ind$prob_die <- die_prob
  
  ################## Initial population created -- check max ID (should be consistent with size) ##########################
  index_to_start <- max(ind$ID)
  
  ############################ Ecological steps j = 1 ... age #############################################################
  foreach(j = seq(1,19,1))%do%{
    if(j<19){
      Fers <- Fertility_list[[j]]
      Mots <- Mortality_list[[j]]}
    else{Fers <- Fertility_list[[18]]
    Mots <- Mortality_list[[18]]}
    
    ########### Migration ############################################################################
    ind <- create_new_stage(ind, TT, Fers, Mots)
    
    ########## Aging ###############################################################################
    ind <- get_older(ind,TT,Fers,Mots)
    
    ######### Sex (or reproduction) ##################################################################
    babies_ <- have_sex(ind, TT, Fers, Mots)
    ind <- babies_[[1]]
    newborn_number <- babies_[[2]]
    newborn_cohort <- babies_[[3]]
    
    ####### Create Focal's network of younger sisters ######
    if(j==1){
      Foc_df <- data.frame()
      focal_s <- newborn_cohort%>%dplyr::select(ID,Age,stage,mother_ID,alive)%>%filter(stage==7)
      focal <- focal_s[sample(nrow(focal_s), 1) ,]
      focal$kin <- "focal"
      focal$alive<-1
      focal_birth_stage <- focal$stage
      mother <- ind%>%filter(ID == focal$mother_ID)
      mother <- mother %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      mother$kin <- "mum"
      mother$alive <- 1
      y_sister <- data.frame(ID = -1, Age = 0, stage = "no", mother_ID = "no", alive = 0, kin = "sister younger")
      Foc_df <- rbind(focal, mother, y_sister)
      Foc_df$year <- j-1 ## By construction, the first ID in the sisters is -1 as there are none yet!
    } 
    else{
      temp_foc <- filter(ind, ID == focal$ID )
      temp_foc <- temp_foc %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_foc)>0){
        temp_foc$kin <- "focal"
        temp_foc$year <- j-1}
      else{temp_foc <- data.frame(ID = focal$ID, Age = j - 1, stage = 0, mother_ID = focal$mother_ID, alive = 0, kin = "focal")
      temp_foc$year <- j-1}
      temp_mum <- filter(ind, ID == focal$mother_ID)
      temp_mum <- temp_mum %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_mum)>0 ){
        temp_mum$kin <- "mum"
        temp_mum$year <- j-1}
      else{temp_mum <- data.frame(ID = focal$mother_ID, Age = 0, stage = 0, mother_ID = "no", alive = 0,  kin = "mum")
      temp_mum$year <- j-1}
      temp_sis <- filter(ind, mother_ID == focal$mother_ID & ID != focal$ID  & Age < temp_foc$Age)
      temp_sis <-  temp_sis %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_sis)>0){
        temp_sis$kin <- "sister younger"
        temp_sis$year <- j-1}
      else{temp_sis <- data.frame(ID = -1, Age = 0, stage = 0, mother_ID = "no", alive = 0,  kin = "sister younger")
      temp_sis$year <- j-1}
      Foc_df <- rbind(Foc_df, rbind(temp_foc, temp_mum, temp_sis)) 
    }
    
    ########### Deaths #############################################################################
    dead_ <- do_deaths(ind)
    ind <- dead_[[1]]
    deceased_coh <- dead_[[2]]
    
    
  }
  
  ### Focal's network for each replication #############################
  Foc_df1 <- Foc_df
  Foc_df1$sim_no <- reps
  Foc_df1$IC <- focal_birth_stage
  sim_list[[(1+length(sim_list))]] <- Foc_df1
}

full_simulation <- do.call("rbind", sim_list)
full_simulation1 <- full_simulation%>%dplyr::select(ID,Age,stage,alive,kin,year,sim_no,IC)

full_simulation2 <- full_simulation1%>%group_by(alive, stage, kin, year, sim_no, IC)%>%
  summarise(alive = sum(alive))%>%
  ungroup()

full_simulation2 <- full_simulation2%>%filter(kin == "sister younger" , stage != 0 &  stage!= "no" )%>%
  group_by(year, kin, stage, IC)%>%
  summarise(expected_val = sum(alive)/no_reps)%>%
  ungroup()

full_simulation2%>%
  ggplot(aes(x = (year), y = expected_val, color = stage, fill = stage)) + theme_bw() +
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Focal's age") + ylab("Expected younger sisters") + ggtitle("... replicates of stoch B-D process") +
  xlim(c(0,19)) + ylim(c(0,0.25))
no_reps
df_out <- here::here("Outputs", "Comparisons", "Time variant", "data frames")
fs::dir_create(df_out)

saveRDS(full_simulation2, file = paste0(df_out , "/" , "YS_IC7_TV.Rds" ))

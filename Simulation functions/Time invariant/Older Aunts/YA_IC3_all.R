
source(here::here("Simulation functions","Demographic events","have_sex.R"))
source(here::here("Simulation functions","Demographic events","change_stage.R"))
source(here::here("Simulation functions","Demographic events","kill_individuals.R"))
source(here::here("Simulation functions","Demographic events","get_older.R"))


no_reps <- 1000
sim_list <- list()
for(reps in 1:no_reps){
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
    return(Fers[[stage]][age])}) )
  ind$prob_have_sex_and_child <- babies_prob
  die_prob <- unlist(lapply(1:nrow(ind), function(x){
    age <- ind$Age[x] + 1
    stage <- ind$stage[x]
    return(1-Mots[[stage]][age])}) )
  ind$prob_die <- die_prob
  
  ################## Initial population created -- check max ID (should be consistent with size) ##########################
  index_to_start <- max(ind$ID)
  
  ############################ Ecological steps j = 1 ... age #############################################################
  for(j in seq(1,34,1)){
    
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
    if(j==15){
      Foc_df <- data.frame()
      focal_s <- newborn_cohort%>%dplyr::select(ID,Age,stage,mother_ID,alive)%>%filter(stage==3)
      focal <- focal_s[sample(nrow(focal_s), 1) ,]
      #focal <- focal_s
      focal$kin <- "focal"
      focal$alive<-1
      focal_birth_stage <- focal$stage
      mother <- ind%>%filter(ID == focal$mother_ID)
      mother <- mother %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      mother$kin <- "mum"
      mother$alive <- 1
      
      older_aunts <- filter(ind, mother_ID == mother$mother_ID  )
      older_aunts <- filter(older_aunts, ID != mother$ID  )
      odler_aunts <- filter(older_aunts, older_aunts$Age < mother$Age)
      older_aunts <- older_aunts %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(older_aunts>0)){
        older_aunts$kin = "aunts"
        older_aunts$alive <- 1}
      else{
        older_aunts <- data.frame(ID = NA, Age = NA, stage = NA, mother_ID = NA, alive = 0, kin = "aunts")}
      
      Foc_df <- rbind(focal, mother, older_aunts)
      Foc_df$year <- j-1 ## By construction, the first ID in the sisters is -1 as there are none yet!
    } 
    else{
      temp_foc <- filter(ind, ID %in% focal$ID )
      temp_foc <- temp_foc %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_foc)>0){
        temp_foc$kin <- "focal"
        temp_foc$year <- j-1}
      else{temp_foc <- data.frame(ID = focal$ID, Age = NA, stage = NA, mother_ID = focal$mother_ID, alive = 0, kin = "focal")
      temp_foc$year <- j-1}
      
      temp_mum <- filter(ind, ID %in% focal$mother_ID)
      temp_mum <- temp_mum %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_mum)>0 ){
        temp_mum$kin <- "mum"
        temp_mum$year <- j-1}
      else{temp_mum <- data.frame(ID = focal$mother_ID, Age = (mother$Age + (j-15)), stage = NA, mother_ID = NA, alive = 0,  kin = "mum")
      temp_mum$year <- j-1}
      
      temp_older_aunts <- filter(ind, mother_ID == mother$mother_ID & ID!= mother$ID)
      temp_older_aunts <- filter(temp_older_aunts, Age < temp_mum$Age)
      temp_older_aunts <- temp_older_aunts %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_older_aunts)>0){
        temp_older_aunts$kin <- "aunts"
        temp_older_aunts$year <- j-1}
      else{temp_older_aunts <- data.frame(ID = NA, Age = NA, stage = NA, mother_ID = NA, alive = 0,  kin = "aunts")
      temp_older_aunts$year <- j-1}
      
      
      Foc_df <- rbind(Foc_df, rbind(temp_foc, temp_mum, temp_older_aunts)) 
    }
    
    ########### Deaths #############################################################################
    dead_ <- do_deaths(ind)
    ind <- dead_[[1]]
    deceased_coh <- dead_[[2]]
  }
  
  ### Focal's network for each replication #############################
  Foc_df1 <- Foc_df
  Foc_df1$sim_no <- reps
  Foc_df1$IC <- rep(unique(focal_birth_stage),nrow(Foc_df1))
  sim_list[[(1+length(sim_list))]] <- Foc_df1
}

full_simulation <- do.call("rbind", sim_list)

full_simulation1 <- full_simulation%>%dplyr::select(ID,Age,stage,alive,kin,year,sim_no,IC)

full_simulation1%>%filter(alive==1 &  ( kin=="aunts" | kin == "mum")  )

full_simulation1%>%filter( (kin == "mum" | kin == "aunts") , !is.na(Age))

full_simulation1%>%subset(kin == "aunts" & Age > 10)

full_simulation2 <- full_simulation1%>%
  group_by(year, kin, stage, IC)%>%
  summarise(expected_val = sum(alive)/no_reps)%>%
  ungroup()

full_simulation2%>%filter(kin == "aunts")%>%
  ggplot(aes(x = (year-15), y = expected_val, color = stage, fill = stage)) + theme_bw() +
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Focal's age") + ylab("Expected same age") + ggtitle("... replicates of stoch B-D process") +
  xlim(c(-1,18)) + ylim(c(0,0.5))


df_out <- here::here("Examples", "ONS data", "Age and Stage", "Time invariant"  , "saved dataframes")
fs::dir_create(df_out)

saveRDS(full_simulation2, file = paste0(df_out , "/" , "SS_sim_stage_FULL_5000_strict_IC7.Rds" ))

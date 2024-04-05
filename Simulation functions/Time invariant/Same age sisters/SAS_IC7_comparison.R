
source(here::here("Simulation functions","Demographic events","have_sex.R"))
source(here::here("Simulation functions","Demographic events","change_stage.R"))
source(here::here("Simulation functions","Demographic events","kill_individuals.R"))
source(here::here("Simulation functions","Demographic events","get_older.R"))


stages_vector <- c(1,2,3,4,5,6,7)
no_replicaions <- 100 ### check code using 5
sim_list<-list()

no_reps <- 5000
sim_list <- list()
foreach(reps = 1:no_reps)%do%{
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
  foreach(j = seq(1,19,1))%do%{
    
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
      focal1 <- focal_s[sample(nrow(focal_s), 1) ,]
      focal1$alive<-1
      focal1$kin <- "focal"
      focal_birth_stage <- focal1$stage
      other_stages_foc <- stages_vector[-focal1$stage]
      focal_complement <- data.frame(ID = rep(focal1$ID,length(other_stages_foc)),
                                     Age = rep(0,length(other_stages_foc)), 
                                     stage = other_stages_foc,
                                     mother_ID = focal1$mother_ID,
                                     alive = rep(0,length(other_stages_foc)), 
                                     kin = rep("focal",length(other_stages_foc)))
      focal <-rbind(focal1,focal_complement)
      
      mother1 <- ind%>%filter(ID == focal1$mother_ID)
      mother1 <- mother1 %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      mother1$alive <- 1
      mother1$kin <- "mum"
      other_stages_mum <- stages_vector[-mother1$stage]
      mother_complement <- data.frame(ID = rep(mother1$ID,length(other_stages_mum)), 
                                      Age = rep(mother1$Age,length(other_stages_mum)),
                                      stage = other_stages_mum, 
                                      mother_ID = rep(NA,length(other_stages_mum)), 
                                      alive = rep(0,length(other_stages_mum)), 
                                      kin = rep("mum",length(other_stages_mum)))
      mother <-rbind(mother1,mother_complement)
      
      sa_sister <- list()
      sa_sister1 <- filter(ind, mother_ID == focal$mother_ID  )
      sa_sister1 <- filter(sa_sister1 , ID != focal1$ID)
      sa_sister1 <- sa_sister1 %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(sa_sister1>0)){
        foreach(i = 1:nrow(sa_sister1))%do%{
        other_stages_sistv <- stages_vector[-sa_sister1[i,]$stage]
        sis_complement_tv <- data.frame(ID = rep(sa_sister1[i,]$ID,length(other_stages_sistv)), 
                                        Age = rep(sa_sister1[i,]$Age,length(other_stages_sistv)), 
                                        stage = other_stages_sistv, 
                                        mother_ID = rep(sa_sister1[i,]$mother_ID,length(other_stages_sistv)),
                                        alive = rep(0,length(other_stages_sistv)))
        sa_sister[[i]] <-rbind(sa_sister1[i,],sis_complement_tv)}
        sa_sister <- do.call("rbind", sa_sister)
        sa_sister$kin <- "sister same age"
        ID_sisters <- unique(sa_sister$ID)}
      else{sa_sister <- data.frame(ID = rep(NA,length(stages_vector)), 
                                  Age = rep(NA,length(stages_vector)), 
                                  stage = stages_vector, 
                                  mother_ID = rep(NA,length(stages_vector)), 
                                  alive = rep(0,length(stages_vector)), 
                                  kin = rep("sister same age",length(stages_vector)))}
      Foc_df <- rbind(focal, mother, sa_sister)
      Foc_df$year <- j-1 ## By construction, the first ID in the sisters is -1 as there are none yet!
    } 
    else{
      temp_foc1 <- filter(ind, ID == focal1$ID )
      temp_foc1 <- temp_foc1 %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      
      if(nrow(temp_foc1)>0){
        other_stages_foc_tv <- stages_vector[-temp_foc1$stage]
        temp_foc_complement <- data.frame(ID = rep(focal1$ID,length(other_stages_foc_tv)), 
                                          Age = rep(temp_foc1$Age,length(other_stages_foc_tv)), 
                                          stage = other_stages_foc_tv, 
                                          mother_ID = rep(focal1$mother_ID,length(other_stages_foc_tv)), 
                                          alive = rep(0,length(other_stages_foc_tv)))
        temp_foc <- rbind(temp_foc1,temp_foc_complement)
        temp_foc$kin <- "focal"}
      else{temp_foc <- data.frame(ID = rep(NA,length(stages_vector)), 
                                  Age = rep((j - 1) ,length(stages_vector)), 
                                  stage = stages_vector, 
                                  mother_ID = rep(focal1$mother_ID,length(stages_vector)),
                                  alive = rep(0,length(stages_vector)),
                                  kin = rep("focal",length(stages_vector)))}
      
      temp_mum1 <- filter(ind, ID == focal$mother_ID)
      temp_mum1 <- temp_mum1 %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_mum1)>0 ){
        other_stages_mumtv <- stages_vector[-temp_mum1$stage]
        mother_complement_tv <- data.frame(ID = rep(mother1$ID,length(other_stages_mumtv)), 
                                           Age = rep(temp_mum1$Age,length(other_stages_mumtv)), 
                                           stage = other_stages_mumtv, 
                                           mother_ID = rep(mother1$ID,length(other_stages_mumtv)), 
                                           alive = rep(0,length(other_stages_mumtv)))
        temp_mum <-rbind(temp_mum1,mother_complement_tv)
        temp_mum$kin <- "mum"}
      else{temp_mum <- data.frame(ID = rep(NA,length(stages_vector)), 
                                  Age = rep(NA,length(stages_vector)), 
                                  stage = stages_vector, 
                                  mother_ID = rep(NA,length(stages_vector)), 
                                  alive = rep(0,length(stages_vector)),  
                                  kin = rep("mum",length(stages_vector)))}
      
      temp_sis <- list()
      temp_sis1 <- ind%>%filter(ID %in% sa_sister$ID)
      temp_sis1 <- temp_sis1 %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_sis1)>0){
        foreach(i = 1:nrow(temp_sis1))%do%{
          other_stages_sistv <- stages_vector[-temp_sis1[i,]$stage]
          sis_complement_tv <- data.frame(ID = rep(temp_sis1[i,]$ID,length(other_stages_sistv)), 
                                          Age = rep(temp_sis1[i,]$Age,length(other_stages_sistv)), 
                                          stage = other_stages_sistv, 
                                          mother_ID = rep(temp_sis1[i,]$mother_ID,length(other_stages_sistv)),
                                          alive = rep(0,length(other_stages_sistv)))
          temp_sis[[i]] <-rbind(temp_sis1[i,],sis_complement_tv)}
        temp_sis <- do.call("rbind",temp_sis)
        temp_sis$kin <- "sister same age"}
      else{temp_sis <- data.frame(ID = rep(NA, length(stages_vector)), 
                                  Age = rep(NA,length(stages_vector)), 
                                  stage = stages_vector, 
                                  mother_ID = rep(NA,length(stages_vector)), 
                                  alive = rep(0,length(stages_vector)),  
                                  kin = rep("sister same age",length(stages_vector)))}
      
      
      temp_foc$year <- j-1
      temp_mum$year <- j-1
      temp_sis$year <- j-1
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

full_simulation2 <- full_simulation2%>%filter( (kin == "sister same age" | kin =="focal" ) , stage != 0 &  stage!= "no" )%>%
  group_by(year, kin, stage, IC)%>%
  summarise(expected_val = sum(alive)/no_reps)%>%
  ungroup()

full_simulation2%>%
  ggplot(aes(x = (year), y = expected_val, color = stage, fill = stage)) + theme_bw() +
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Focal's age") + ylab("Expected same age") + ggtitle("... replicates of stoch B-D process") +
  xlim(c(0,18)) + ylim(c(0,1.1))


df_out <- here::here("Examples", "ONS data", "Age and Stage", "Time invariant"  , "saved dataframes")
fs::dir_create(df_out)

saveRDS(full_simulation2, file = paste0(df_out , "/" , "SS_sim_stage_FULL_5000_strict_IC7.Rds" ))

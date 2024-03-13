
source(here::here("Simulation functions","Demographic events","have_sex.R"))
source(here::here("Simulation functions","Demographic events","change_stage.R"))
source(here::here("Simulation functions","Demographic events","kill_individuals.R"))
source(here::here("Simulation functions","Demographic events","get_older.R"))


########## record the changing population structure

df_ages <- rep(seq(0,18,1),each=7)
df_stages <- rep(seq(1,7,1), 19)


no_replicaions <- 5000 ### check code using 5
sim_list<-list()
foreach(reps = 1:no_replicaions)%do%{
  
  # record the time-varying "stable (or not so) population structure"
  
  age_stage_df <- data.frame(Ages =df_ages, Stages = df_stages)
  age_stage_df$age_stage <- paste0(age_stage_df$Ages , "-" , age_stage_df$Stages)
  age_stage_df$New_age_stage <- NA
  transient_SPD <- data.frame(age_stage = paste0(age_stage_df$Ages , "-" , age_stage_df$Stages))
  transient_SPD$num_age_stage <- seq(1,133,1)
  
  
  ## create a fictional age-structured pop, with inds given unique ID
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
    return((1-Mots[[stage]][age]))}) )
  ind$prob_die <- die_prob
  
  full_sis <- list()## need to keep track of the unique IDs that Focal's sisters have
  foreach(j = seq(1, 30, 1))%do%{
    
    ########### Migration ############################################################################
    ind <- create_new_stage(ind, TT, Fers, Mots)
    
    ########## Aging ###############################################################################
    ind <- get_older(ind,TT,Fers,Mots)
    
    ######### Sex (or reproduction) ##################################################################
    babies_ <- have_sex_lapply(ind, TT, Fers, Mots)
    ind <- babies_[[1]]
    newborn_number <- babies_[[2]]
    newborn_cohort <- babies_[[3]]
    
    
    ### Update the stable population structure -- after births and before deaths?
    age_stage_df$New_age_stage <- lapply(1:nrow(age_stage_df), function(x){
      sum( ind$Age == age_stage_df$Age[x] & ind$stage == age_stage_df$Stage[x] ) } )
    normalised_transient_SPD <- age_stage_df$New_age_stage%>%unlist()
    transient_SPD[,(2+j)] <- normalised_transient_SPD/(sum(normalised_transient_SPD))
    
    ############ Here I pick some random focal, and its mother, and subsequently track them through the simulation
    ### For the first year focal, by definition has a mother, but no sister
    
    if(j==8){
      #if(is.null(born_F)){break}
      Foc_df <- data.frame()
      focal_s <- newborn_cohort%>%dplyr::select(ID,Age,stage,mother_ID,alive)%>%filter(stage == 3)
      focal <- focal_s[sample(nrow(focal_s), 1) ,]
      focal$kin <- "focal"
      focal$alive <- 1
      focal_birth_stage <- focal$stage
      mother <- ind%>%filter(ID == focal$mother_ID)
      mother <- mother %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      mother$kin <- "mum"
      mother$alive <- 1
      
      o_sister <- ind%>%filter(mother_ID == focal$mother_ID)
      o_sister <- o_sister%>%filter(Age > focal$Age)
      o_sister <- o_sister%>%dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(o_sister)>0){
        ID_sisters <- o_sister$ID
        o_sister$kin <- "sister older"}
      else{o_sister <- data.frame(ID = -1, Age = 0, stage = "no", mother_ID = "no", alive = 0, kin = "sister older")}
      
      Foc_df <- rbind(focal, mother, o_sister)
      Foc_df$year <- j-1 ## By construction, the first ID in the sisters is -1 as there are none yet!
    } 
    ### For years > 1 focal can die, lose mother, gain and lose sisters
    else{
      temp_foc <- filter(ind, ID == focal$ID & mother_ID == focal$mother_ID)
      temp_foc <- temp_foc%>%dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_foc)>0){
        temp_foc$kin <- "focal"
        temp_foc$year <- j-1}
      else{temp_foc <- data.frame(ID = focal$ID, Age = 0, stage = "no", mother_ID = focal$mother_ID, alive = 0, kin = "focal")
      temp_foc$year <- j-1}
      
      temp_mum <- filter(ind, ID == focal$mother_ID)
      temp_mum <- temp_mum%>%dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_mum)>0 ){
        temp_mum$kin <- "mum"
        temp_mum$year <- j-1}
      else{temp_mum <- data.frame(ID = focal$mother_ID, Age = 0, stage = "no", mother_ID = "no", alive = 0, kin = "mum")
      temp_mum$year <- j-1}
      
      temp_sis <- ind%>%filter(ID %in% o_sister$ID)
      temp_sis <- temp_sis %>% dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(temp_sis)>0){
        temp_sis$kin <- "sister older"
        temp_sis$year <- j-1}
      else{temp_sis <- data.frame(ID = -1, Age = 0, stage = "no", mother_ID = "no", alive = 0, kin = "sister older")
      temp_sis$year <- j-1}
      
      Foc_df <- rbind(Foc_df, rbind(temp_foc, temp_mum, temp_sis)) 
      
    }
    ### End of focals network!
    
    ############ Deaths####################################
    dead_ <- do_deaths(ind)
    ind <- dead_[[1]]
    deceased_coh <- dead_[[2]]
    
    
    
  }
  ## replication number of focals network (for ensemble average)
  Foc_df1 <- Foc_df
  Foc_df1$sim_no <- reps
  Foc_df1$IC <- focal_birth_stage
  sim_list[[(1+length(sim_list))]] <- Foc_df1
  
  transient_SPD_list1 <- transient_SPD
  transient_SPD_list1$sim_no <- reps
  transient_SPD_list[[reps]] <-  transient_SPD_list1
  
}
full_simulation%>%head()
full_simulation <- do.call("rbind", sim_list)
full_simulation1 <- full_simulation%>%dplyr::select(ID,Age,stage,alive,kin,year,sim_no,IC,mother_ID)

full_simulation2 <- full_simulation1%>%filter(kin == "sister older" , stage != 0 &  stage!= "no" )%>%
  group_by(year, stage, IC)%>%
  summarise(expected_val = sum(alive)/no_replicaions)%>%
  ungroup()

full_simulation2%>%
  ggplot(aes(x = (year-7), y = expected_val, color = stage, fill = stage)) + theme_bw() +
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Focal's age") + ylab("Expected older sisters") + ggtitle("... replicates of stoch B-D process") +
  xlim(c(0,18)) + ylim(c(0,0.5))

reps

df_out <- here::here("Outputs", "Time invariant"  , "saved dataframes")
fs::dir_create(df_out)

saveRDS(full_simulation2, file = paste0(df_out , "/" , "OS_sim_TI_IC3.Rds" ))
#

################# Export the stable population structure ####################

plot(transient_SPD$V9)
transient_SPD%>%head()
plot(marg_age_dist(19,7,sps))
plot(marg_age_dist(19,7,transient_SPD$V3))
plot(marg_age_dist(19,7,transient_SPD$V5))
plot(marg_age_dist(19,7,transient_SPD$V7))
plot(marg_age_dist(19,7,transient_SPD$V28))


transient_SPD_list_full <- do.call("rbind", transient_SPD_list)
head(transient_SPD_list_full)
ncol(transient_SPD_list_full)
transient_SPD_list_full <- transient_SPD_list_full[,c(1,2,3,4,5,6,7,8,9,10,28)]

colnames(transient_SPD_list_full) <- c("age_stage", "num_age_stage" , "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "sim_no")

transient_SPD_list_full%>%head()
transient_SPD_list_full$age_stage%>%length()

transient_SPD_list_full <- transient_SPD_list_full%>%mutate(age_stage  = reorder(age_stage,  num_age_stage) )

transient_SPD_list_full <- transient_SPD_list_full%>%group_by(age_stage, num_age_stage)%>%
  summarise(y1 = mean(y1),
            y2 = mean(y2),
            y3 = mean(y3),
            y4 = mean(y4),
            y5 = mean(y5),
            y6 = mean(y6),
            y7 = mean(y7),
            y8 = mean(y8))%>%ungroup()


plot(transient_SPD_list_full$y7)
plot(sps)
transient_SPD_list_full%>%head()

div_stp <- data.frame(SPD = sps)
for(i in 1 : 8){
  div_stp[,(1+i)] <- transient_SPD_list_full[,(2+i)]
}
plot(div_stp$y1)
plot(div_stp$y7)
sum(div_stp$y2)

div_stp%>%head()
colnames(div_stp) <- c("stable" , "year1" , "year2" ,"year3","year4" , "year5" ,"year6","year7" ,"year8")

div_stp$age_stage_num <- transient_SPD_list_full$age_stage
div_stp$age_stage_num1 <- transient_SPD_list_full$num_age_stage

div_stp%>%head()

#div_stp<-readr::read_rds(here::here("Examples", "ONS data","Age and Stage","Time invariant", "saved dataframes" , "TV_pop_structure_10_steps.Rds"))

div_stp%>%dplyr::select(stable,year7,age_stage_num1)%>%
  melt(id = "age_stage_num1")%>%ggplot(aes(x = age_stage_num1, y = value, color = variable)) +
  geom_point()


df_out <- here::here("Outputs","Time invariant", "saved dataframes")
fs::dir_create(df_out)

saveRDS(div_stp, file = paste0(df_out , "/" , "Pop_structure_OS_TI_IC3.Rds" ))


df_marg_age <- data.frame(stable = rep(NA,19))

df_marg_age$stable <- marg_age_dist(19,7,div_stp$stable)[,1]
df_marg_age$year1 <- marg_age_dist(19,7,div_stp$year1)[,1]
df_marg_age$year2 <- marg_age_dist(19,7,div_stp$year2)[,1]
df_marg_age$year3 <- marg_age_dist(19,7,div_stp$year3)[,1]
df_marg_age$year4 <- marg_age_dist(19,7,div_stp$year4)[,1]
df_marg_age$year5 <- marg_age_dist(19,7,div_stp$year5)[,1]
df_marg_age$year6 <- marg_age_dist(19,7,div_stp$year6)[,1]
df_marg_age$year7 <- marg_age_dist(19,7,div_stp$year7)[,1]
df_marg_age$year8 <- marg_age_dist(19,7,div_stp$year8)[,1]

df_marg_age$age <- seq(0,18,1)

colnames(df_marg_age) <- c("stable" , "year1" , "year2" ,"year3","year4" , "year5" ,"year6","year7","year8" , "age")

df_marg_age%>%dplyr::select(age, stable,year1,year2,year3,year4,year5,year6,year7)%>%
  melt(id = c("age"))

pp<-df_marg_age%>%dplyr::select(age, stable,year2,year4,year6,year8)%>%
  melt(id = c("age"))%>%
  ggplot(aes(x = age*5, y= value, color = variable))  +
  xlab("ages in population") + ylab("population freq")  +
  geom_point() + theme_bw() + scale_color_viridis_d() +theme(text = element_text(size = 15)) +
  ylim(c(0,0.12)) 



plt_out <- here::here("Outputs", "Time invariant"  , "Figs", "Theory_vs_sim_SPS")
fs::dir_create(plt_out)



ggsave(pp, file = paste0(plt_out , "/" , "diverging_SPS_IC7_5000.png" ), width = 8, height = 5, dpi = 300)


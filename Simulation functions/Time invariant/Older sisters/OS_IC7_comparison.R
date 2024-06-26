
source(here::here("Simulation functions","Demographic events","have_sex.R"))
source(here::here("Simulation functions","Demographic events","change_stage.R"))
source(here::here("Simulation functions","Demographic events","kill_individuals.R"))
source(here::here("Simulation functions","Demographic events","get_older.R"))


########## record the changing population structure

stages_vector <- c(1,2,3,4,5,6,7)
df_ages <- rep(seq(0,18,1),each=7)
df_stages <- rep(seq(1,7,1), 19)
df_stages
transient_SPD_list <- list()
no_replicaions <- 100 ### check code using 5
no_replicaions_actual <- no_replicaions
sim_list<-list()

for(reps in 1:no_replicaions){
  
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
  
  for(j in seq(1, 30, 1)){
    
    ########### Migration ############################################################################
    ind <- create_new_stage(ind, TT, Fers, Mots)
    ########## Aging ###############################################################################
    ind <- get_older(ind,TT,Fers,Mots)
    ######### Sex (or reproduction) ##################################################################
    babies_ <- have_sex(ind, TT, Fers, Mots)
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
      if(sum(newborn_number)==0){
        no_replicaions_actual <- no_replicaions_actual - 1
        break}
      if(nrow(newborn_cohort%>%dplyr::select(ID,Age,stage,mother_ID,alive)%>%filter(stage==7))==0){
        no_replicaions_actual <- no_replicaions_actual - 1
        break}
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
      
      o_sister <- list()
      o_sister1 <- ind%>%filter(mother_ID == focal$mother_ID)
      o_sister1 <- o_sister1%>%filter(Age > focal1$Age)
      o_sister1 <- o_sister1%>%dplyr::select(ID,Age,stage,mother_ID,alive)
      if(nrow(o_sister1)>0){
        foreach(i = 1:nrow(o_sister1))%do%{
          other_stages_sistv <- stages_vector[-o_sister1[i,]$stage]
          sis_complement_tv <- data.frame(ID = rep(o_sister1[i,]$ID,length(other_stages_sistv)), 
                                          Age = rep(o_sister1[i,]$Age,length(other_stages_sistv)), 
                                          stage = other_stages_sistv, 
                                          mother_ID = rep(o_sister1[i,]$mother_ID,length(other_stages_sistv)),
                                          alive = rep(0,length(other_stages_sistv)))
          o_sister[[i]] <-rbind(o_sister1[i,],sis_complement_tv)}
        o_sister <- do.call("rbind", o_sister)
        o_sister$kin <- "sister older"
        ID_sisters <- unique(o_sister$ID)}
      else{o_sister <- data.frame(ID = rep(NA,length(stages_vector)), 
                                  Age = rep(NA,length(stages_vector)), 
                                  stage = stages_vector, 
                                  mother_ID = rep(NA,length(stages_vector)), 
                                  alive = rep(0,length(stages_vector)), 
                                  kin = rep("sister older",length(stages_vector)))}
      
      Foc_df <- rbind(focal, mother, o_sister)
      Foc_df$year <- j-1 ## By construction, the first ID in the sisters is -1 as there are none yet!
      } 
    ### For years > 8 focal can die, lose mother, gain and lose sisters
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
      temp_sis1 <- ind%>%filter(ID %in% o_sister$ID)
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
        temp_sis$kin <- "sister older"}
      else{temp_sis <- data.frame(ID = rep(NA, length(stages_vector)), 
                                  Age = rep(NA,length(stages_vector)), 
                                  stage = stages_vector, 
                                  mother_ID = rep(NA,length(stages_vector)), 
                                  alive = rep(0,length(stages_vector)),  
                                  kin = rep("sister older",length(stages_vector)))}
      
      
      temp_foc$year <- j-1
      temp_mum$year <- j-1
      temp_sis$year <- j-1
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
  
  #transient_SPD_list1 <- transient_SPD
  #transient_SPD_list1$sim_no <- reps
  #transient_SPD_list[[reps]] <-  transient_SPD_list1
  
}
no_replicaions_actual
full_simulation <- do.call("rbind", sim_list)
full_simulation1 <- full_simulation%>%dplyr::select(ID,Age,stage,alive,kin,year,sim_no,IC,mother_ID)

full_simulation2 <- full_simulation1%>%filter(kin == "sister older" , stage != 0 &  stage!= "no" )%>%
  group_by(year, stage, IC)%>%
  summarise(expected_val = sum(alive)/no_replicaions_actual)%>%
  ungroup()

full_simulation2%>%
  ggplot(aes(x = (year-6), y = expected_val, color = stage, fill = stage)) + theme_bw() +
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Focal's age") + ylab("Expected older sisters") + ggtitle("... replicates of stoch B-D process") +
  xlim(c(0,18)) + ylim(c(0,0.25))

reps

df_out <- here::here("Outputs", "Comparisons", "Time invariant", "data frames")
fs::dir_create(df_out)

saveRDS(full_simulation2, file = paste0(df_out , "/" , "OS_TI_IC7_comparison.Rds" ))


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

pp

plt_out <- here::here("Outputs", "Time invariant"  , "Figs", "Theory_vs_sim_SPS")
fs::dir_create(plt_out)



ggsave(pp, file = paste0(plt_out , "/" , "diverging_SPS_IC3_5000.png" ), width = 8, height = 5, dpi = 300)


library(dplyr)
library(tidyr)
library(haven)
library(sjlabelled)
library(DHS.rates)
library(questionr)
library(stringr)

women <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Ghana/PMA2014_GHR3_HHQFQ_v1_23Dec2016.dta")

women_clean <- women %>% filter(!is.na(FQweight) )

# Method Prevelance (used to calculte method mix)
# want to check variable coding for v312 (method)
Labels=get_labels(women_clean$current_methodnum)
Var1=get_values(women_clean$current_methodnum)
Methods=as.data.frame(cbind(Labels, Var1))
#pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category

women_clean <- women_clean %>% mutate(method=case_when(current_methodnum==7 ~ "Pill",
                                                       current_methodnum==5 |  current_methodnum==6 ~ "Injectable",
                                                       current_methodnum==4 ~ "IUD",
                                                       current_methodnum==3 ~ "Implant",
                                                       current_methodnum==9 ~ "MCondom",
                                                       current_methodnum==10 ~ "FCondom",
                                                       current_methodnum==14 ~ "LAM",
                                                       current_methodnum==1 ~ "FSter",
                                                       current_methodnum==2 ~ "MSter",
                                                       current_methodnum==13 ~ "SDM",
                                                       current_methodnum==8 | current_methodnum==11 | current_methodnum==12 | current_methodnum==15 | current_methodnum==19 ~ "OMM",
                                                       current_methodnum==30 | current_methodnum==31 | current_methodnum==32 | current_methodnum==39  ~ "Traditional",
                                                       cp==0 ~ "None"))

# Method Information Index Plus
# fp_side_effects, fp_side_effects_instructions (only if fp_side_effects is yes), fp_told_other_methods, fp_told_switch
women_clean <- women_clean %>% mutate(told_se=case_when(fp_side_effects==1 ~ 1, fp_side_effects!=1 ~ 0),
                                      told_todo_se=case_when(fp_side_effects==1 & fp_side_effects_instructions==1 ~ 1, 
                                                             fp_side_effects==1 & fp_side_effects_instructions==0 ~ 0,
                                                             fp_side_effects!=1 ~ 0),
                                      told_om = case_when(fp_told_other_methods==1 ~ 1, fp_told_other_methods!=1 ~ 0),
                                      mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                                                    told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0))

women_clean <- women_clean %>% mutate(Past_Intention=case_when(pregnancy_last_desired==1 ~ "Then",
                                                               pregnancy_last_desired==2 | pregnancy_last_desired==3 ~ "Mistimed"))

women_clean <- women_clean %>% mutate(Future_Intention=case_when( more_children==1 | more_children== -88 ~ "Another/Undecided",
                                                                  more_children==3 | more_children==2  ~ "NoMore/Other", 
                                                                  more_children_pregnant==1 | more_children_pregnant== -88 ~ "Another/Undecided",
                                                                  more_children_pregnant==3 | more_children_pregnant==2  ~ "NoMore/Other"))

women_clean <- women_clean %>% mutate(Labels=case_when(mcp==1 & fp_provider>=10 & fp_provider<=19 ~ "Public",
                                                       mcp==1 & fp_provider>=20 & fp_provider<=29 ~ "Private",
                                                       mcp==1 & fp_provider==35  ~ "Private",
                                                       mcp==1 & fp_provider>=30  ~ "Other",
                                                       mcp==1 ~ "Other",
                                                       mcp!=1 ~ "Not Using Modern Method"))
sel <- select(women_clean, mcp, Labels, fp_provider)
###########################################################################

women_clean <- women_clean %>% mutate(Year_Interview=as.numeric(str_sub(FQdoi_correctedSIF, 1,4)),
                                      Month_Interview =as.numeric(str_sub(FQdoi_correctedSIF, 6,7)),
                                      CMC_Interview=((Year_Interview-1900) * 12) + Month_Interview,
                                      Year_RecentBirth=as.numeric(str_sub(recent_birthSIF, 1,4)),
                                      Month_RecentBirth =as.numeric(str_sub(recent_birthSIF, 6,7)),
                                      CMC_RecentBirth=((Year_RecentBirth-1900) * 12) + Month_RecentBirth)
sel <- women_clean %>% select(FQdoi_corrected , FQdoi_correctedSIF, Year_Interview, Month_Interview, CMC_Interview, recent_birth, recent_birthSIF, Year_RecentBirth, Month_RecentBirth, CMC_RecentBirth)

women_clean <- women_clean %>% mutate(timesincebirth= CMC_Interview-CMC_RecentBirth)

women_recentbirth <- women_clean %>% filter(timesincebirth<=12)
women_recentbirth_users <- women_recentbirth %>% filter(!is.na(mii)) %>% filter(mcp==1)
women_users <- women_clean %>% filter(!is.na(mii))  %>% filter(mcp==1)

sel <- select(women_clean, mcp, mii, method, fp_side_effects , fp_side_effects_instructions, fp_told_other_methods  )

##############################################################################

use14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), weights = women_recentbirth$FQweight))) %>% filter(Var1==1) %>% 
  rename(PP_MCP=Freq) %>% select(-Var1) %>% mutate(SurveyID="GHPMA14") %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2014)
method_use14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$method), weights = women_recentbirth$FQweight))) %>% rename(PP_MethPrev=Freq)  %>% mutate(SurveyID="GHPMA14") %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2014)
source_use14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$Labels), weights = women_recentbirth$FQweight))) %>% rename(PP_MethPrev=Freq)  %>% mutate(SurveyID="GHPMA14") %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2014)

mii_recent14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth_users$mii), weights = women_recentbirth_users$FQweight))) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA14") %>% select(-Var1) %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2014)
mii_n <- as.data.frame(table(women_recentbirth_users$method)) %>% rename(Var2=Var1, N=Freq)

mii_method_recent14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth_users$mii), y=as.factor(women_recentbirth_users$method), weights = women_recentbirth_users$FQweight),2)) %>%
  left_join(mii_n, by="Var2") %>% filter(N>=25) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA14") %>% select(-Var1)  %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2014)


mii_all14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_users$mii), weights = women_users$FQweight))) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA14") %>% select(-Var1) %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2014)
mii_all_n <- as.data.frame(table(women_users$method)) %>% rename(Var2=Var1, N=Freq)

mii_method_all14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_users$mii), y=as.factor(women_users$method), weights = women_users$FQweight),2)) %>%
  left_join(mii_all_n, by="Var2") %>% filter(N>=25) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA14") %>% select(-Var1)  %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2014)


use_past_intention14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Past_Intention), weights = women_recentbirth$FQweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq) %>% mutate(SurveyID="GhanaPMA 2014") %>% select(-Var1)
use_future_intention14 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Future_Intention), weights = women_recentbirth$FQweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq) %>% mutate(SurveyID="GhanaPMA 2014") %>% select(-Var1)

##############################################################################
##############################################################################
##############################################################################


women <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Ghana/PMA2018_GHR6_HHQFQ_v1_19Feb2019/PMA2017_GHR6_HHQFQ_v1_19Feb2019.dta")

women_clean <- women %>% filter(!is.na(FQweight) )

# Method Prevelance (used to calculte method mix)
# want to check variable coding for v312 (method)
Labels=get_labels(women_clean$current_methodnum)
Var1=get_values(women_clean$current_methodnum)
Methods=as.data.frame(cbind(Labels, Var1))
#pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category

women_clean <- women_clean %>% mutate(method=case_when(current_methodnum==7 ~ "Pill",
                                                       current_methodnum==5 |  current_methodnum==6 |  current_methodnum==16 ~ "Injectable",
                                                       current_methodnum==4 ~ "IUD",
                                                       current_methodnum==3 ~ "Implant",
                                                       current_methodnum==9 ~ "MCondom",
                                                       current_methodnum==10 ~ "FCondom",
                                                       current_methodnum==14 ~ "LAM",
                                                       current_methodnum==1 ~ "FSter",
                                                       current_methodnum==2 ~ "MSter",
                                                       current_methodnum==13 ~ "SDM",
                                                       current_methodnum==8 | current_methodnum==11 | current_methodnum==12 | current_methodnum==15 | current_methodnum==19 ~ "OMM",
                                                       current_methodnum==30 | current_methodnum==31 | current_methodnum==32 | current_methodnum==39  ~ "Traditional",
                                                       cp==0 ~ "None"))

# Method Information Index Plus
# fp_side_effects, fp_side_effects_instructions (only if fp_side_effects is yes), fp_told_other_methods, fp_told_switch
women_clean <- women_clean %>% mutate(told_se=case_when(fp_side_effects==1 ~ 1, fp_side_effects!=1 ~ 0),
                                      told_todo_se=case_when(fp_side_effects==1 & fp_side_effects_instructions==1 ~ 1, 
                                                             fp_side_effects==1 & fp_side_effects_instructions==0 ~ 0,
                                                             fp_side_effects!=1 ~ 0),
                                      told_om = case_when(fp_told_other_methods==1 ~ 1, fp_told_other_methods!=1 ~ 0),
                                      mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                                                    told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0))

women_clean <- women_clean %>% mutate(Past_Intention=case_when(pregnancy_last_desired==1 ~ "Then",
                                                               pregnancy_last_desired==2 | pregnancy_last_desired==3 ~ "Mistimed"))

women_clean <- women_clean %>% mutate(Future_Intention=case_when( more_children==1 | more_children== -88 ~ "Another/Undecided",
                                                                  more_children==3 | more_children==2  ~ "NoMore/Other", 
                                                                  more_children_pregnant==1 | more_children_pregnant== -88 ~ "Another/Undecided",
                                                                  more_children_pregnant==3 | more_children_pregnant==2  ~ "NoMore/Other"))
women_clean <- women_clean %>% mutate(Labels=case_when(mcp==1 & fp_provider_rw>=10 & fp_provider_rw<=19 ~ "Public",
                                                       mcp==1 & fp_provider_rw>=20 & fp_provider_rw<=29 ~ "Private",
                                                       mcp==1 & fp_provider_rw==35  ~ "Private",
                                                       mcp==1 & fp_provider_rw>=30  ~ "Other",
                                                       mcp==1 ~ "Other",
                                                       mcp!=1 ~ "Not Using Modern Method"))
###########################################################################

women_clean <- women_clean %>% mutate(Year_Interview=as.numeric(str_sub(FQdoi_correctedSIF, 1,4)),
                                      Month_Interview =as.numeric(str_sub(FQdoi_correctedSIF, 6,7)),
                                      CMC_Interview=((Year_Interview-1900) * 12) + Month_Interview,
                                      Year_RecentBirth=as.numeric(str_sub(recent_birthSIF, 1,4)),
                                      Month_RecentBirth =as.numeric(str_sub(recent_birthSIF, 6,7)),
                                      CMC_RecentBirth=((Year_RecentBirth-1900) * 12) + Month_RecentBirth)
sel <- women_clean %>% select(FQdoi_corrected , FQdoi_correctedSIF, Year_Interview, Month_Interview, CMC_Interview, recent_birth, recent_birthSIF, Year_RecentBirth, Month_RecentBirth, CMC_RecentBirth)

women_clean <- women_clean %>% mutate(timesincebirth= CMC_Interview-CMC_RecentBirth)

women_recentbirth <- women_clean %>% filter(timesincebirth<=12)
women_recentbirth_users <- women_recentbirth %>% filter(!is.na(mii)) %>% filter(mcp==1)
women_users <- women_clean %>% filter(!is.na(mii)) %>% filter(mcp==1)

##############################################################################

use17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), weights = women_recentbirth$FQweight))) %>% filter(Var1==1) %>% 
  rename(PP_MCP=Freq) %>% select(-Var1) %>% mutate(SurveyID="GHPMA17") %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2017)
method_use17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$method), weights = women_recentbirth$FQweight))) %>% rename(PP_MethPrev=Freq)  %>% mutate(SurveyID="GHPMA17") %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2017)
source_use17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$Labels), weights = women_recentbirth$FQweight))) %>% rename(PP_MethPrev=Freq)  %>% mutate(SurveyID="GHPMA17") %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2017)

mii_recent17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth_users$mii), weights = women_recentbirth_users$FQweight))) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA17") %>% select(-Var1) %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2017)
mii_n <- as.data.frame(table(women_recentbirth_users$method)) %>% rename(Var2=Var1, N=Freq)

mii_method_recent17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth_users$mii), y=as.factor(women_recentbirth_users$method), weights = women_recentbirth_users$FQweight),2)) %>%
  left_join(mii_n, by="Var2") %>% filter(N>=25) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA17") %>% select(-Var1)  %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2017)

mii_all17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_users$mii), weights = women_users$FQweight))) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA17") %>% select(-Var1) %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2017)
mii_all_n <- as.data.frame(table(women_users$method)) %>% rename(Var2=Var1, N=Freq)

mii_method_all17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_users$mii), y=as.factor(women_users$method), weights = women_users$FQweight),2)) %>%
  left_join(mii_all_n, by="Var2") %>% filter(N>=25) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID="GHPMA17") %>% select(-Var1)  %>%
  mutate(Country="Ghana") %>% mutate(StartYear=2017)

use_past_intention17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Past_Intention), weights = women_recentbirth$FQweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq) %>% mutate(SurveyID="GhanaPMA 2017") %>% select(-Var1)
use_future_intention17 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Future_Intention), weights = women_recentbirth$FQweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq) %>% mutate(SurveyID="GhanaPMA 2017") %>% select(-Var1)


ghana_use <- bind_rows(use14, use17)
ghana_methoduse <- bind_rows(method_use14, method_use17)
ghana_sourceuse <- bind_rows(source_use14, source_use17)

ghana_mii <- bind_rows(mii_recent14, mii_recent17)
ghana_mii_method <- bind_rows(mii_method_recent14, mii_method_recent17)

ghana_mii_all <- bind_rows(mii_all14, mii_all17)
ghana_mii_method_all <- bind_rows(mii_method_all14, mii_method_all17)

ghana_use_past_intention_all <- bind_rows(use_past_intention14, use_past_intention17)
ghana_use_future_intention_all <- bind_rows(use_future_intention14, use_future_intention17)

write.csv(ghana_use, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Use 20132019.csv", row.names = F, na="")
write.csv(ghana_methoduse, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Method Use 20132019.csv", row.names = F, na="")
write.csv(ghana_mii, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum MII 20132019.csv", row.names = F, na="")
write.csv(ghana_mii_method, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum MII Method 20132019.csv", row.names = F, na="")
write.csv(ghana_mii_all, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA All MII 20132019.csv", row.names = F, na="")
write.csv(ghana_mii_method_all, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA All MII Method 20132019.csv", row.names = F, na="")
write.csv(ghana_use_past_intention_all, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Use by Past Intentions 20132019.csv", row.names = F, na="")
write.csv(ghana_use_future_intention_all, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Use by Future Intentions 20132019.csv", row.names = F, na="")
write.csv(ghana_sourceuse, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Source Use 20142017.csv", row.names = F, na="")

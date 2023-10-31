# Track20 PPFP Presentation
# Nepal November 2023

# For questions regarding this code, please contact Kristin Bietsch kbietsch@avenirhealth.org

library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(sjlabelled)
library(questionr)
library(ggplot2)
library(ggrepel)
library(tidyverse)

library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

facility_births <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/DeliverFacility2Year.csv") %>% filter(!is.na(Survey))

dhs_list <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/DHS Surveys for Request.csv") %>% filter(!is.na(StartYear)) %>%
  mutate(IRFile=paste(Survey, ".dta", sep=""))

# Removing Ghana and Malawi
dhs_list <- dhs_list %>% filter(Country!="Malawi")  %>% filter(Country!="Ghana")  %>% filter(Country!="Uganda")

dhs_info <- dhs_list %>% select(Country, ISONum, StartYear, IRFile) %>% rename(SurveyID=IRFile)

# Bring in Malawi (2) and Ghana (4)

malawi_use <- read.csv( "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Malawi MICS Post Partum Use 20132019.csv")
malawi_method_use <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Malawi MICS Post Partum Method Use 20132019.csv")
ghana_use <- read.csv( "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Use 20132019.csv")
ghana_method_use <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Method Use 20132019.csv")
ghana_mii_recent <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum MII 20132019.csv")
ghana_mii_method_recent <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum MII Method 20132019.csv")
ghana_mii_all <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA All MII 20132019.csv")
ghana_mii_method_all <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA All MII Method 20132019.csv")
ghana_sourceuse <- read.csv( "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Ghana PMA Post Partum Source Use 20142017.csv") %>%
  rename(Labels_clean=Var1) %>% select(SurveyID, Labels_clean, PP_MethPrev, Country, StartYear)

#dhs_list <- dhs_list[1,]
results.use <- setNames(data.frame(matrix(ncol = 2,  nrow = 0)),  c("PP_MCP" ,   "SurveyID")) %>% mutate( PP_MCP=as.numeric(PP_MCP), SurveyID=as.character(SurveyID))
results.method_use <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c("Var1","PP_MethPrev" ,   "SurveyID")) %>% mutate( Var1=as.character(Var1), PP_MethPrev=as.numeric(PP_MethPrev), SurveyID=as.character(SurveyID))
results.mii_recent <- setNames(data.frame(matrix(ncol = 2,  nrow = 0)),  c("PP_MII" ,   "SurveyID")) %>% mutate( PP_MII=as.numeric(PP_MII), SurveyID=as.character(SurveyID))
results.mii_method_recent <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)),  c("Var2", "PP_MII" ,  "N", "SurveyID")) %>% mutate(Var2=as.character(Var2), PP_MII=as.numeric(PP_MII), N=as.numeric(N),  SurveyID=as.character(SurveyID))
results.mii_all <- setNames(data.frame(matrix(ncol = 2,  nrow = 0)),  c("PP_MII" ,   "SurveyID")) %>% mutate( PP_MII=as.numeric(PP_MII), SurveyID=as.character(SurveyID))
results.mii_method_all <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)),  c("Var2", "PP_MII" ,  "N", "SurveyID")) %>% mutate(Var2=as.character(Var2), PP_MII=as.numeric(PP_MII), N=as.numeric(N),  SurveyID=as.character(SurveyID))
results.source_use <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)),  c("Var1","PP_MethPrev" ,   "SurveyID", "Labels")) %>% mutate( Var1=as.numeric(Var1), PP_MethPrev=as.numeric(PP_MethPrev), SurveyID=as.character(SurveyID), Labels=as.character(Labels))

setwd("C:/Users/KristinBietsch/files/DHSLoop")



for (row in 1:nrow(dhs_list)) {
  data <- dhs_list[row, "IRFile"]
  country <- dhs_list[row, "Country"]
  year <- dhs_list[row, "StartYear"]
  
  
  women <- read_dta(data, col_select = c(v005, v008, b3_01, v312, v313,  v502 ,  v3a02, v3a04, v3a05, v327))
  
  # Setup data
  women_clean <- women %>% mutate(sampleweights=v005/100000,
                                  married=case_when(v502==1 ~ 1, v502!=1 ~ 0),
                                  unmarried=case_when(v502!=1 ~ 1, v502==1 ~ 0))
  
  
  women_clean <- women_clean %>% mutate(mcp=case_when(v313==3 ~ 1, v313!=3 ~ 0),
                                        tcp=case_when(v313==1 | v313==2 ~ 1, v313==0 | v313==3 ~ 0))
  
  
  
  # Method Prevelance (used to calculte method mix)
  # want to check variable coding for v312 (method)
  Labels=get_labels(women$v312)
  Var1=get_values(women$v312)
  Methods=as.data.frame(cbind(Labels, Var1))
  #pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category
  
  women_clean <- women_clean %>% mutate(method=case_when(v312==1 ~ "Pill",
                                                         v312==3 ~ "Injectable",
                                                         v312==2 ~ "IUD",
                                                         v312==11 ~ "Implant",
                                                         v312==5 ~ "MCondom",
                                                         v312==14 ~ "FCondom",
                                                         v312==13 ~ "LAM",
                                                         v312==6 ~ "FSter",
                                                         v312==7 ~ "MSter",
                                                         v312==18 ~ "SDM",
                                                         v312==16 | v312==17 ~ "OMM",
                                                         v312==8 | v312==9 | v312==10 ~ "Trad",
                                                         v312==0 ~ "Not Using",
                                                         TRUE ~ "Other"))
  
  
  
  # Source
  Labels=get_labels(women$v327)
  Var1=get_values(women$v327)
  Source=as.data.frame(cbind(Labels, Var1)) %>% mutate(Var1=as.numeric(as.character(Var1)))
  
  
  women_clean <- women_clean %>% mutate(method_source=case_when(!is.na(v327) ~ v327,
                                                                mcp==1 ~ 99,
                                                                mcp!=1 ~ 0))
  
  #sel <- women_clean %>% select(mcp, v327, method_source)
  
  
  
  # Method Information Index (MII+ not available for this survey)
  #v3a02 v3a04 (only if v3a02 is yes) v3a05
  
  if (exists("v3a02", women_clean) & !is.na(sum(women_clean$v3a02, na.rm=T)) & sum(women_clean$v3a02, na.rm=T)!=0) {
    
    women_clean <- women_clean %>% mutate(told_se=case_when(v3a02==1 ~ 1, v3a02!=1 ~ 0),
                                          told_todo_se=case_when(v3a02==1 & v3a04==1 ~ 1, 
                                                                 v3a02==1 & v3a04==0 ~ 0,
                                                                 v3a02!=1 ~ 0),
                                          told_om = case_when(v3a05==1 ~ 1, v3a05!=1 ~ 0),
                                          mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                                                        told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0))
    
  }
  
  # Give Birth in Last Year
  women_clean <- women_clean %>% mutate(timesincebirth= v008-b3_01)
  
  #######################################################################################
  women_recentbirth <- women_clean %>% filter(timesincebirth<=12)
  
  if (exists("v3a02", women_clean) & !is.na(sum(women_clean$v3a02, na.rm=T)) & sum(women_clean$v3a02, na.rm=T)!=0) {
    women_recentbirth_users <- women_recentbirth %>% filter(!is.na(mii))
    women_users <- women_clean %>% filter(!is.na(mii))
  }
  #######################################################################################
  
  use <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), weights = women_recentbirth$sampleweights))) %>% filter(Var1==1) %>% rename(PP_MCP=Freq) %>% mutate(SurveyID=data) %>% select(-Var1)
  method_use <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$method), weights = women_recentbirth$sampleweights))) %>% rename(PP_MethPrev=Freq) %>% mutate(SurveyID=data)
  source_use <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$method_source), weights = women_recentbirth$sampleweights))) %>% rename(PP_MethPrev=Freq) %>% 
    mutate(SurveyID=data) %>% mutate(Var1=as.numeric(as.character(Var1))) %>% left_join(Source, by="Var1") %>% mutate(Labels=case_when(Var1==0 ~ "Not Using Modern Method", Var1==99 ~ "No Source", TRUE ~ Labels))
    
  
  
  
  if (exists("v3a02", women_clean) & !is.na(sum(women_clean$v3a02, na.rm=T)) & sum(women_clean$v3a02, na.rm=T)!=0) {
    
    mii_recent <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth_users$mii), weights = women_recentbirth_users$sampleweights))) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID=data) %>% select(-Var1)
    mii_n <- as.data.frame(table(women_recentbirth_users$method)) %>% rename(Var2=Var1, N=Freq)
    
    mii_method_recent <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth_users$mii), y=as.factor(women_recentbirth_users$method), weights = women_recentbirth_users$sampleweights),2)) %>%
      left_join(mii_n, by="Var2") %>% filter(N>=25) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID=data) %>% select(-Var1)
    
    
    mii_all <- as.data.frame(prop.table(wtd.table(x= as.factor(women_users$mii), weights = women_users$sampleweights))) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID=data) %>% select(-Var1)
    mii_all_n <- as.data.frame(table(women_users$method)) %>% rename(Var2=Var1, N=Freq)
    
    mii_method_all <- as.data.frame(prop.table(wtd.table(x= as.factor(women_users$mii), y=as.factor(women_users$method), weights = women_users$sampleweights),2)) %>%
      left_join(mii_all_n, by="Var2") %>% filter(N>=25) %>% filter(Var1==1) %>% rename(PP_MII=Freq) %>% mutate(SurveyID=data) %>% select(-Var1)
    
  }
  ######################################################
  
  results.use <- bind_rows(results.use, use)
  results.method_use <- bind_rows(results.method_use , method_use)
  results.source_use <- bind_rows(results.source_use, source_use)
  if (exists("v3a02", women_clean) & !is.na(sum(women_clean$v3a02, na.rm=T)) & sum(women_clean$v3a02, na.rm=T)!=0) {
    
    results.mii_recent <- bind_rows(results.mii_recent , mii_recent)
    results.mii_method_recent <- bind_rows(results.mii_method_recent, mii_method_recent) 
    
    results.mii_all <- bind_rows(results.mii_all , mii_all)
    results.mii_method_all <- bind_rows(results.mii_method_all, mii_method_all) 
    
  }
  
  
}



######################################################################################
results.use_clean <- results.use %>% left_join(dhs_info, by="SurveyID") %>%
  bind_rows(malawi_use, ghana_use) %>%
  mutate(ISONum=case_when(Country=="Malawi" ~ 454,
                          Country=="Ghana" ~ 288,
                          TRUE ~ ISONum))

results.use_clean.labels <- results.use_clean %>% group_by(Country) %>% mutate(Min=min(StartYear)) %>% filter(StartYear==Min)

not_mod <- c( "Not Using",  "Other",  "Trad" ,  "None" ,  "Other" ,   "Periodic Abstinence/Rhythm",   "Traditional"  , "Withdrawal"  )

results.method_use_clean <- results.method_use %>% left_join(dhs_info, by="SurveyID")   %>%
  bind_rows(malawi_method_use, ghana_method_use) %>% filter(!Var1 %in% not_mod) %>%
  mutate(Var1=case_when( Var1=="F_Condom"  ~ "FCondom" ,
                         Var1== "F_Ster"  ~      "FSter" ,   
                         Var1=="M_Condom" ~  "MCondom",
                         Var1=="M_Ster"   ~     "MSter"  ,
                         TRUE ~ Var1))
levels(as.factor(results.method_use_clean$Var1))


results.mii_recent_clean <- results.mii_recent %>% left_join(dhs_info, by="SurveyID") %>%
  bind_rows( ghana_mii_recent) %>% mutate(Var2="Total") 

results.mii_method_recent_clean <- results.mii_method_recent %>% left_join(dhs_info, by="SurveyID") %>%
  select(PP_MII, SurveyID, Country, StartYear, Var2) %>%
  bind_rows( ghana_mii_method_recent)

results.mii_graph <- bind_rows(results.mii_recent_clean, results.mii_method_recent_clean) %>% 
  filter(Var2!="MCondom") %>% 
  filter(Var2!="Other") %>%
  mutate(Var2=case_when(Var2=="FSter" ~ "Female Sterilization",
                        TRUE ~ Var2)) %>%
  mutate(Var2=factor(Var2)) %>% 
  mutate(Var2=fct_relevel(Var2, c("Total" , "Female Sterilization", "Implant"  ,            "Injectable",           "IUD"  ,                "Pill"     ))) 

results.mii_graph_labels <- results.mii_graph %>% group_by(Var2, Country) %>%
  mutate(Min=min(StartYear)) %>% filter(Min==StartYear)



results.mii_all_clean <- results.mii_all %>% left_join(dhs_info, by="SurveyID") %>%
  bind_rows( ghana_mii_all) %>% mutate(Var2="Total") 

results.mii_method_all_clean <- results.mii_method_all %>% left_join(dhs_info, by="SurveyID") %>%
  select(PP_MII, SurveyID, Country, StartYear, Var2) %>%
  bind_rows( ghana_mii_method_all)


results.mii_all_graph <- bind_rows(results.mii_all_clean, results.mii_method_all_clean) %>% 
  filter(Var2!="MCondom") %>% 
  filter(Var2!="Other") %>%
  mutate(Var2=case_when(Var2=="FSter" ~ "Female Sterilization",
                        TRUE ~ Var2)) %>%
  mutate(Var2=factor(Var2)) %>% 
  mutate(Var2=fct_relevel(Var2, c("Total" , "Female Sterilization", "Implant"  ,            "Injectable",           "IUD"  ,                "Pill"     )))  %>%
  rename(All_MII=PP_MII) %>%
  select(All_MII, Country, StartYear, Var2)

comparison_mii <- full_join(results.mii_graph, results.mii_all_graph, by=c("Country", "StartYear", "Var2"))
total_comparison_mii <- comparison_mii %>% filter(Var2=="Total") %>%
  arrange(Country, StartYear) %>%
  select(Country, StartYear, Var2, SurveyID, All_MII, PP_MII) %>%
  gather(Variable, Value, All_MII:PP_MII) %>%
  mutate(Variable=case_when(Variable=="All_MII" ~ "All Users",
                            Variable=="PP_MII" ~ "Post Partum Users"))

fster_comparison_mii <- comparison_mii %>% filter(Var2=="Female Sterilization") %>%
  arrange(Country, StartYear) %>%
  select(Country, StartYear, Var2, SurveyID, All_MII, PP_MII) %>%
  gather(Variable, Value, All_MII:PP_MII)

Implant_comparison_mii <- comparison_mii %>% filter(Var2=="Implant") %>%
  arrange(Country, StartYear) %>%
  select(Country, StartYear, Var2, SurveyID, All_MII, PP_MII) %>%
  gather(Variable, Value, All_MII:PP_MII)

Injectable_comparison_mii <- comparison_mii %>% filter(Var2=="Injectable") %>%
  arrange(Country, StartYear) %>%
  select(Country, StartYear, Var2, SurveyID, All_MII, PP_MII) %>%
  gather(Variable, Value, All_MII:PP_MII)

IUD_comparison_mii <- comparison_mii %>% filter(Var2=="IUD") %>%
  arrange(Country, StartYear) %>%
  select(Country, StartYear, Var2, SurveyID, All_MII, PP_MII) %>%
  gather(Variable, Value, All_MII:PP_MII)

pill_comparison_mii <- comparison_mii %>% filter(Var2=="Pill") %>%
  arrange(Country, StartYear) %>%
  select(Country, StartYear, Var2, SurveyID, All_MII, PP_MII) %>%
  gather(Variable, Value, All_MII:PP_MII)

ggplot(total_comparison_mii, aes(x=StartYear, y=Value*100, color=Variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, scales="free_x",  ncol = 7) +
  labs(title="Method Information Index,\nWomen who Gave Birth in the Last Year and All Users", y="Method Information Index", x="", color="") +
  theme_bw()  +
  theme(strip.text = element_text(size=18),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        legend.text = element_text(size=18),
        title=element_text(size=18),
        legend.position = "bottom")
ggsave("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/MII Comparison.png", height = 7, width=15, units = "in")

ggplot(fster_comparison_mii, aes(x=StartYear, y=Value, color=Variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, scales="free_x") 


ggplot(Implant_comparison_mii, aes(x=StartYear, y=Value, color=Variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, scales="free_x") 


ggplot(Injectable_comparison_mii, aes(x=StartYear, y=Value, color=Variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, scales="free_x") 


ggplot(IUD_comparison_mii, aes(x=StartYear, y=Value, color=Variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, scales="free_x") 


ggplot(pill_comparison_mii, aes(x=StartYear, y=Value, color=Variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, scales="free_x") 


levels(as.factor(results.source_use$Labels))

results.source_use_clean <- results.source_use %>%
  mutate(Labels_clean =case_when(Labels== "government (hospital/phc/hp/bhcc/uhc/chu/inst fp)"     |
                                   Labels== "government clinic/pharmacy"  |                          
                                   Labels== "Government clinic/pharmacy" |                          
                                   Labels== "government home/community delivery"   |                
                                   Labels== "Government home/community delivery" |                   
                                   Labels== "government home/community delivery - fchv/mobile camp" |
                                   Labels=="govt clinical/pharm"  |                                 
                                   Labels=="govt home/comm deliv" ~ "Public"  ,
                                 Labels== "ngo"  |                                                                                    
                                   Labels=="pharmacy"  |                                            
                                   Labels=="private clin/deliv"  |                                 
                                   Labels=="private clinic/delivery" |                              
                                   Labels=="private pharmacy" ~ "Private",
                                 Labels=="No Source"  |                                          
                                   Labels=="other"  |                                              
                                   Labels=="Other" | 
                                   Labels== "don't know" |                                           
                                   Labels=="Don't know"  |                                          
                                   Labels=="drug vendor, shop, friend"   |                         
                                   Labels=="shop, church, friend" |                                 
                                   Labels=="Shop, church, friend"  ~ "Other" , 
                                 Labels=="Not Using Modern Method"   ~ "Not Using Modern Method",
                                 is.na(Labels) ~ "Other")) %>%
  group_by(SurveyID, Labels_clean) %>% summarise(PP_MethPrev=sum(PP_MethPrev)) %>% left_join(dhs_info, by="SurveyID") %>%
  bind_rows(ghana_sourceuse) %>%
  filter(Labels_clean!="Not Using Modern Method") 

fill_colors <- c(  "Other"="#FF8E8C",    "Private"="#D2EFFF",      "Public"="#70AD47" )

ggplot(results.source_use_clean, aes(x=as.factor(StartYear), y=PP_MethPrev*100, fill=Labels_clean)) +
  geom_bar(stat="identity", color="black") +
  facet_wrap(~Country, scales="free_x") +
  labs(title="Percent Using a Modern Method (By Source),\nWomen who Gave Birth in the Last Year", y="Modern Method Use", x="", fill="") +
  scale_fill_manual(values=fill_colors) +
  theme_bw() +
  theme(
    strip.text = element_text(size=18),
    strip.background = element_blank(),
    legend.text = element_text(size=18),
    title=element_text(size=18),
    axis.text = element_text(size=18))

ggsave("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/PPFP Source 103123.png", height = 10, width=10, units = "in")

######################################################################################
ggplot(results.use_clean, aes(x=StartYear, y=100*PP_MCP, color=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2010, 2022, 4)) + 
  geom_text_repel(data=results.use_clean.labels, aes(x=StartYear, y=100*PP_MCP, label=Country), size=6) +
  labs(title="Percent Using a Modern Method, Women who Gave Birth in the Last Year", y="Modern Method Use", x="") +
  theme_bw() +
  theme( axis.text = element_text(size=14),
         legend.position = "none",
         title = element_text(size=15))

ggsave("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Trends PP Use DHS.png", height = 8, width=10, units = "in")
write.csv(results.use_clean, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Trends PP Use DHS.csv", na="", row.names = F)

levels(as.factor(results.method_use_clean$Var1))

fill_colors <- c( "Diaphragm"="#FFE2AF",  "FCondom"="#FF8E8C",    "FSter"="#D2EFFF",      "Implant"="#70AD47",    "Injectable"="#242071", "IUD" ="#CEF9EC",       "LAM"="#20B1FE",        "MCondom"="#FF4340",    "MSter" ="#BFBFBF",     "OMM" ="#E2F0D9",       "Pill" ="#FFB636",      "SDM" ="#C7BEFF" )

ggplot(results.method_use_clean, aes(x=as.factor(StartYear), y=PP_MethPrev*100, fill=Var1)) +
  geom_bar(stat="identity", color="black") +
  facet_wrap(~Country, scales="free_x") +
  labs(title="Percent Using a Modern Method (By Method), Women who Gave Birth in the Last Year", y="Modern Method Use", x="", fill="") +
  scale_fill_manual(values=fill_colors) +
  theme_bw() +
  theme(
    strip.text = element_text(size=18),
    strip.background = element_blank(),
    legend.text = element_text(size=18),
    title=element_text(size=18),
    axis.text = element_text(size=18))
ggsave("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Trends PP Use By Method DHS.png", height = 13, width=15, units = "in")
write.csv(results.method_use_clean, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Trends PP Use By Method DHS.csv", na="", row.names = F)

ggplot(results.mii_graph, aes(x=StartYear, y=100*PP_MII, color=Country)) +
  geom_line() +
  geom_point() +
  geom_text_repel(data=results.mii_graph_labels, aes(x=StartYear, y=100*PP_MII, label=Country), size=6) +
  facet_wrap(~Var2) +
  scale_x_continuous(breaks=seq(2010, 2022, 4)) + 
  labs(title="Method Information Index, Women who Gave Birth in the Last Year", y="Method Information Index", x="", caption="Minimum 25 Users per Survey") +
  theme_bw() +
  theme(strip.text = element_text(size=18),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16),
        strip.background = element_blank(),
        legend.position = "none",
        plot.caption=element_text(size=14),
        title=element_text(size=16))
ggsave("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Trends MII By Method DHS.png", height = 10, width=15, units = "in")
write.csv(results.mii_graph, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Trends MII By Method DHS.csv", na="", row.names = F)

facility_births_recent <- facility_births %>% group_by(Country) %>%
  mutate(Max=max(Survey)) %>%
  filter(Max==Survey)

ggplot(facility_births, aes(x=Survey, y=HealthFacility, color=Country)) +
  geom_line() +
  geom_point() +
  geom_text_repel(data=facility_births_recent, aes(x=Survey, y=HealthFacility, label=Country), size=6) +
  labs(title="Births in Health Facilities", y="Percent", x="", caption="Births within 2 Years of Survey") +
  scale_x_continuous(breaks=seq(2007, 2022, 5)) +
  theme_bw() +
  theme( axis.text = element_text(size=14),
         legend.position = "none",
         plot.caption=element_text(size=14),
         title = element_text(size=15))

ggsave("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Trends Facility Births DHS.png", height = 8, width=10, units = "in")

################################
use_change <- results.use_clean %>% arrange(Country, -StartYear) %>% group_by(Country) %>% mutate(Min=min(StartYear), Max=max(StartYear)) %>%
  filter(StartYear==Min | StartYear==Max) %>%
  mutate(id= row_number()) %>% mutate(id=paste("Survey", id, sep="")) %>%
  select(Country, id, PP_MCP) %>%
  spread(id, PP_MCP) %>%
  mutate(Change=Survey1 - Survey2) %>%
  mutate(Change_Group=case_when(Change>0 ~ "Increase",
                                Change<=0 ~ "Decrease"))

table(use_change$Change_Group)

rank_methods <- results.method_use_clean %>%  arrange(Country, Var1, -StartYear) %>% group_by(Country) %>% mutate(Max=max(StartYear))  %>%
  filter(StartYear==Max) %>% arrange(Country, -PP_MethPrev) %>%
  mutate(id= row_number()) %>%
  filter(id==1)
table(rank_methods$Var1)

implants_pp_change <- results.method_use_clean %>%  arrange(Country, Var1, -StartYear)  %>% filter(Var1=="Implant") %>% group_by(Country) %>% mutate(Min=min(StartYear), Max=max(StartYear)) %>%
  filter(StartYear==Min | StartYear==Max) %>%
  mutate(id= row_number()) %>% mutate(id=paste("Survey", id, sep="")) %>%
  select(Country, id, PP_MethPrev) %>%
  spread(id, PP_MethPrev) %>%
  mutate(Change=Survey1 - Survey2) %>%
  mutate(Change_Group=case_when(Change>0 ~ "Increase",
                                Change<=0 ~ "Decrease"))

mii_change <- results.mii_graph %>% arrange(Country, -StartYear) %>% group_by(Country) %>% mutate(Min=min(StartYear), Max=max(StartYear)) %>%
  filter(StartYear==Min | StartYear==Max) %>% arrange(Country, Var2, -StartYear) %>%
  ungroup() %>% group_by(Country, Var2) %>% mutate(n=1) %>%
  mutate(N=sum(n)) %>%
  filter(N==2)  %>%
  mutate(id= row_number()) %>% mutate(id=paste("Survey", id, sep="")) %>%
  select(Country, Var2, id, PP_MII)  %>%
  spread(id, PP_MII) %>%
  mutate(Change=Survey1 - Survey2) %>%
  mutate(Change_Group=case_when(Change>0 ~ "Increase",
                                Change<=0 ~ "Decrease"))



#########################################################################
results.use_recent <- results.use_clean %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>%
  filter(recent==StartYear) %>% rename(iso_n3=ISONum)


options(scipen = 999)


world <- ne_countries(scale = "medium", returnclass = "sf")


world_map <- world %>% mutate(iso_n3=as.numeric(as.character(iso_n3))) %>% left_join(results.use_recent, by="iso_n3")

na_col <- "#CCCCCC"


ggplot(data = world_map) +
  geom_sf(aes(fill = PP_MCP*100)) +
  coord_sf( ylim = c(-40, 40), xlim = c(-20, 160) , expand = FALSE) +
  scale_fill_viridis(option="plasma",
                     na.value = na_col,
                     direction = -1) +
  labs(title="",
       x ="", y = "", fill="") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank())

ggsave( "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Postpartum map 103123.png",  height=8, width=15, units = "in")

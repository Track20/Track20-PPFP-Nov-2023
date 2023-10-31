
library(dplyr)
library(haven)
library(survey)
require(xlsx)
library(tibble)
library(questionr)
library(reshape2)
library(DHS.rates)

options(scipen = 999)

setwd("C:/Users/KristinBietsch/Files/MICS Data")


# Malawi 2013

Country <- "Malawi" 
StartYear <- 2013
ISOCode <- 454

mics <- read_sav("Malawi_MICS5_Datasets/Malawi MICS 2013-14 SPSS Datasets/wm.sav")
sel <- select(mics, SB1, SB3U, SB3N, wmweight)

mics_br <- read_sav("Malawi_MICS5_Datasets/Malawi MICS 2013-14 SPSS Datasets/bh.sav")

library(foreign)
brmics1 <-  read.spss("Malawi_MICS5_Datasets/Malawi MICS 2013-14 SPSS Datasets/bh.sav", to.data.frame= TRUE)
brmics1 <- as.data.frame(attr(brmics1, "variable.labels"))  %>%  tibble::rownames_to_column()

mics1 <-  read.spss("Malawi_MICS5_Datasets/Malawi MICS 2013-14 SPSS Datasets/wm.sav", to.data.frame= TRUE)
mics1 <- as.data.frame(attr(mics1, "variable.labels"))  %>%  tibble::rownames_to_column()

regions <- as.data.frame(attr(mics$HH7,"labels")) %>% rownames_to_column()
#write.csv(regions, "Malawi_MICS5_Datasets/Malawi MICS 2013-14 SPSS Datasets/RegionID.csv", row.names = F)


attr(mics$DB1,"labels")
attr(mics$DB2,"labels")
attr(mics$UN4,"labels")
attr(mics$UN6,"labels")


attr(mics$CP3A,"labels")
attr(mics$CP3B,"labels")
attr(mics$CP3C,"labels")
attr(mics$CP3D,"labels")
attr(mics$CP3E,"labels")
attr(mics$CP3F,"labels")
attr(mics$CP3G,"labels")
attr(mics$CP3H,"labels")
attr(mics$CP3I,"labels")
attr(mics$CP3J,"labels")
attr(mics$CP3K,"labels")
attr(mics$CP3L,"labels")
attr(mics$CP3M,"labels")
attr(mics$CP3X,"labels")

# Contraceptive use
mics <- mics %>% mutate(method=case_when(CP3A=="A" ~ "F_Ster", 
                                         CP3B=="B" ~ "M_Ster",
                                         CP3C=="C" ~ "IUD", 
                                         CP3D=="D" ~ "Injectable",
                                         CP3E=="E" ~ "Implant",
                                         CP3F=="F" ~ "Pill",
                                         CP3G=="G" ~ "M_Condom",
                                         CP3H=="H" ~ "F_Condom",
                                         CP3J=="J" ~ "Foam/Jelly",
                                         CP3L=="L" ~ "Periodic Abstinence/Rhythm",
                                         CP3M=="M" ~ "Withdrawal",
                                         CP3X=="X" ~ "Other",
                                         TRUE ~ "None"),
                        cpr=case_when(method!="None" ~ 1, method=="None" ~ 0),
                        mcp=case_when(method=="None" | method== "Periodic Abstinence/Rhythm" | method=="Withdrawal" | method== "Other" ~ 0,
                                       TRUE ~ 1)) 

mics <- mics %>% mutate(Past_Intention=case_when(DB1==1 ~ "Then",
                                                 DB1==2  ~ "Mistimed"))

mics <- mics %>% mutate(Future_Intention=case_when(   UN6==1 | UN6== 8  ~ "Another/Undecided",
                                                      UN6==3 | UN6==2  ~ "NoMore/Other", 
                                                                  UN4==1 | UN4== 8 ~ "Another/Undecided",
                                                                  UN4==2   ~ "NoMore/Other"))

##############################################################################
# Recent Births
# CMC Last Birth WDOBLC
# CMC Interview WDOI

mics <- mics %>% mutate(timesincebirth= WDOI-WDOBLC)

women_recentbirth <- mics %>% filter(timesincebirth<=12)
##############################################################################

use13 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), weights = women_recentbirth$wmweight))) %>% filter(Var1==1) %>% 
  rename(PP_MCP=Freq) %>% select(-Var1) %>% mutate(SurveyID="MWMICS13") %>%
  mutate(Country="Malawi") %>% mutate(StartYear=2013)
method_use13 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$method), weights = women_recentbirth$wmweight))) %>% rename(PP_MethPrev=Freq)  %>% mutate(SurveyID="MWMICS13") %>%
  mutate(Country="Malawi") %>% mutate(StartYear=2013)

use_past_intention13 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Past_Intention), weights = women_recentbirth$wmweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq)  %>% mutate(SurveyID="MWMICS13") %>%
  mutate(Country="Malawi") %>% mutate(StartYear=2013) %>% select(-Var1)
use_future_intention13 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Future_Intention), weights = women_recentbirth$wmweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq)  %>% mutate(SurveyID="MWMICS13") %>%
  mutate(Country="Malawi") %>% mutate(StartYear=2013) %>% select(-Var1)

########################################################################################


setwd("C:/Users/KristinBietsch/Files/MICS Data")


# Malawi 2019

Country <- "Malawi" 
StartYear <- 2019
ISOCode <- 454

mics <- read_sav("Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/wm.sav")
sel <- select(mics, SB1, SB2U, SB2N, wmweight) %>% filter(wmweight!=0)

mics_br <- read_sav("Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/bh.sav")

library(foreign)
mics1 <-  read.spss("Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/wm.sav", to.data.frame= TRUE)
mics1 <- as.data.frame(attr(mics1, "variable.labels")) %>% rownames_to_column()


attr(mics$DB1,"labels")
attr(mics$DB2,"labels")
attr(mics$UN2,"labels")
attr(mics$UN4,"labels")
# Contraceptive use

mics <- mics %>% mutate(method=case_when(CP4A=="A" ~ "F_Ster", 
                                         CP4B=="B" ~ "M_Ster",
                                         CP4C=="C" ~ "IUD", 
                                         CP4D=="D" ~ "Injectable",
                                         CP4E=="E" ~ "Implant",
                                         CP4F=="F" ~ "Pill",
                                         CP4G=="G" ~ "M_Condom",
                                         CP4H=="H" ~ "F_Condom",
                                         CP4I=="I" ~ "Diaphragm",
                                         CP4J=="J" ~ "Foam/Jelly",
                                         CP4K=="K" ~ "LAM",
                                         CP4L=="L" ~ "Periodic Abstinence/Rhythm",
                                         CP4M=="M" ~ "Withdrawal",
                                         CP4X=="X" ~ "Other",
                                         TRUE ~ "None"),
                        cpr=case_when(method!="None" ~ 1, method=="None" ~ 0),
                        mcp=case_when(method=="None" | method== "Periodic Abstinence/Rhythm" | method=="Withdrawal" | method== "Other" ~ 0,
                                      TRUE ~ 1)) 

#mics <- mics %>% mutate(Past_Intention=case_when(DB1==1 ~ "Then",
#                                                 DB1==2  ~ "Mistimed"))

#mics <- mics %>% mutate(Future_Intention=case_when(   UN2==1 | UN2== 8  ~ "Another/Undecided",
#                                                      UN2==3 | UN2==2  ~ "NoMore/Other", 
#                                                      UN4==1 | UN4== 8 ~ "Another/Undecided",
#                                                      UN4==2   ~ "NoMore/Other"))


mics <- mics %>% mutate(timesincebirth= WDOI-WDOBLC)

women_recentbirth <- mics %>% filter(timesincebirth<=12)

#sel <- select(women_recentbirth , mcp, Future_Intention, UN2, UN4)

##############################################################################

use19 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), weights = women_recentbirth$wmweight))) %>% filter(Var1==1) %>% 
  rename(PP_MCP=Freq) %>% select(-Var1) %>% mutate(SurveyID="MWMICS19") %>%
  mutate(Country="Malawi") %>% mutate(StartYear=2019)
method_use19 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$method), weights = women_recentbirth$wmweight))) %>% rename(PP_MethPrev=Freq)  %>% mutate(SurveyID="MWMICS19") %>%
  mutate(Country="Malawi") %>% mutate(StartYear=2019)

#use_past_intention19 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Past_Intention), weights = women_recentbirth$wmweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq)  %>% mutate(SurveyID="MWMICS19") %>%
#  mutate(Country="Malawi") %>% mutate(StartYear=2019) %>% select(-Var1)
#use_future_intention19 <- as.data.frame(prop.table(wtd.table(x= as.factor(women_recentbirth$mcp), y=as.factor(women_recentbirth$Future_Intention), weights = women_recentbirth$wmweight),2)) %>% filter(Var1==1) %>% rename(PP_MCP=Freq)  %>% mutate(SurveyID="MWMICS19") %>%
#  mutate(Country="Malawi") %>% mutate(StartYear=2019) %>% select(-Var1)

##############################################################################
malawi_use <- bind_rows(use13, use19)
malawi_methoduse <- bind_rows(method_use13, method_use19)

#malawi_use_past_intention <- bind_rows(use_past_intention13, use_past_intention19)
#malawi_use_future_intention <- bind_rows(use_future_intention13, use_future_intention19)

write.csv(malawi_use, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Malawi MICS Post Partum Use 20132019.csv", row.names = F, na="")
write.csv(malawi_methoduse, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Malawi MICS Post Partum Method Use 20132019.csv", row.names = F, na="")
write.csv(use_past_intention13, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Malawi MICS Post Partum Use Past Intention 20132019.csv", row.names = F, na="")
write.csv(use_future_intention13, "C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/Malawi MICS Post Partum Use Future Intention 20132019.csv", row.names = F, na="")

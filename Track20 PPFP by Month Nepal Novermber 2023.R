# Month by Month PPFP, by Method
# Using DHS Calendar Data
# Track20 PPFP Presentation
# Nepal November 2023

# For questions regarding this code, please contact Kristin Bietsch kbietsch@avenirhealth.org

# PPFP code built of original code by Bill Winfrey

##add in necessary packages

library(xlsx)
library(haven)
library(survey)
library(questionr)
library(iotools)
library(devtools)
library(stringr)
library(stringi)
library(dplyr)
library(tidyverse)
library(labelled)


surveys <- read.csv("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/DHS Surveys for Request.csv") %>% filter(!is.na(StartYear)) %>%
  mutate(IRfile=paste(Survey, ".dta", sep=""))

# Removing Uganda
surveys <- surveys   %>% filter(Country!="Uganda")

# Most Recent Survey Only
#surveys <- surveys %>% group_by(Country) %>% mutate(Recent=max(StartYear)) %>% filter(Recent==StartYear) %>% select(-Recent) %>% ungroup()

survey.info <- surveys %>% select(Country, StartYear, API_ID, ISONum) %>% rename(ISOCode=ISONum, Survey=API_ID) 

######################

results_total <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)),  
                          c("Var1" , "Freq", "Month", "Survey")) %>% 
  mutate(Var1=as.character(Var1), Freq=as.numeric(Freq), Month=as.numeric(Month),  Survey=as.character(Survey))
######################

#surveys <- surveys[1:2, ]


for (row in 1:nrow(surveys)) {
  data <- surveys[row, "IRfile"]
  countryname <- surveys[row, "API_ID"]
  countryid <- surveys[row, "Alpha2"]
  evermarried <- surveys[row, "EverMarried"]
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  
  
  women <- read_dta(data, col_select = any_of(c("vcal_1", "v005")))
  
  if (exists("vcal_1", women) ) {
    
    
    data_clean <- women %>% mutate(weight=v005/100000) %>%
      mutate(trim_cal= stri_trim(vcal_1),
             lb=regexpr('B', trim_cal),
             birthl_5=case_when(lb>=13 & lb<=60 ~1 ,
                                lb<13 | lb>60 ~ 0))   %>%
      filter(birthl_5==1) %>% # dropping those without birth 13 to 60 months before the survey
      mutate(pp=mstrsplit(trim_cal, "B", ncol = 10, type=c("character")),
             ppV1 = pp[ ,1],
             pb = stri_reverse(ppV1),
             postbirth = substr(pb, 1, 12),
             postbirth1 = substr(pb, 1, 1),
             postbirth2 = substr(pb, 2, 2),
             postbirth3 = substr(pb, 3, 3),
             postbirth4 = substr(pb, 4, 4),
             postbirth5 = substr(pb, 5, 5),
             postbirth6 = substr(pb, 6, 6),
             postbirth7 = substr(pb, 7, 7),
             postbirth8 = substr(pb, 8, 8),
             postbirth9 = substr(pb, 9, 9),
             postbirth10 = substr(pb, 10, 10),
             postbirth11 = substr(pb, 11, 11),
             postbirth12 = substr(pb, 12, 12))

    results.1 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth1, weights = data_clean$weight))) %>% mutate(Month=1)
    results.2 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth2, weights = data_clean$weight))) %>% mutate(Month=2)
    results.3 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth3, weights = data_clean$weight))) %>% mutate(Month=3)
    results.4 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth4, weights = data_clean$weight))) %>% mutate(Month=4)
    results.5 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth5, weights = data_clean$weight))) %>% mutate(Month=5)
    results.6 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth6, weights = data_clean$weight))) %>% mutate(Month=6)
    results.7 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth7, weights = data_clean$weight))) %>% mutate(Month=7)
    results.8 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth8, weights = data_clean$weight))) %>% mutate(Month=8)
    results.9 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth9, weights = data_clean$weight))) %>% mutate(Month=9)
    results.10 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth10, weights = data_clean$weight))) %>% mutate(Month=10)
    results.11 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth11, weights = data_clean$weight))) %>% mutate(Month=11)
    results.12 <- as.data.frame(prop.table(wtd.table(x=data_clean$postbirth12, weights = data_clean$weight))) %>% mutate(Month=12)
    


    
    results <- bind_rows(results.1, results.2, results.3, results.4, results.5, results.6, results.7, results.8, results.9, results.10, results.11, results.12) %>% mutate(Survey=countryname) 
    
    results_total <- bind_rows(results_total, results)
    
  }
  
}

table(results_total$Var1)

results_clean <- results_total %>%
  mutate(Method= case_when(Var1=="" | Var1=="?" | Var1=="*"  ~ "Missing",
                           Var1=="0" ~ "None",
                           Var1=="1" ~ "Pill",
                           Var1=="2" ~ "IUD",
                           Var1=="3" ~ "Injectable",
                           Var1=="4" ~ "Diaphram",
                           Var1=="5" ~ "Condom",
                           Var1=="6" ~ "FSter",
                           Var1=="7" ~ "MSter",
                           Var1=="8" ~ "PerAbstin",
                           Var1=="9" ~ "Withdrawal",
                           Var1=="a" ~ "CountrySpecific",
                           Var1=="C" ~ "FCondom",
                           Var1=="D" ~ "CountrySpecific",
                           Var1=="E" ~ "EC",
                           Var1=="F" ~ "Foam",
                           Var1=="G" ~ "CountrySpecific",
                           Var1=="H" ~ "CountrySpecific",
                           Var1=="I" ~ "CountrySpecific",
                           Var1=="K" ~ "CountrySpecific",
                           Var1=="L" ~ "LAM",
                           Var1=="M" ~ "OMM",
                           Var1=="N" ~ "Implants",
                           Var1=="P" ~ "None",
                           Var1=="Q" ~ "CountrySpecific",
                           Var1=="R" ~ "CountrySpecific",
                           Var1=="S" ~ "SDM",
                           Var1=="T" ~ "None",
                           Var1=="W" ~ "OtherTrad",
                           TRUE ~ "Missing"))  %>%
  select(-Var1) %>%
  group_by(Survey, Month, Method, Freq ) %>%
  summarise(Freq=sum(Freq)) %>% 
  filter(Method!="None") %>% ungroup() %>%
  mutate(Method_Group=case_when(Method=="Condom"  | Method=="FCondom"   ~ "Condom",
                                Method=="CountrySpecific"   | Method=="Diaphram"     | Method==     "EC"      | Method==       "SDM"     | Method==         "Foam"     | Method==    "OMM"   ~ "Other Modern",    
                                Method=="FSter"   | Method==    "MSter"   ~ "Sterilization",       
                                Method=="Implants"  ~ "Implants",    
                                Method=="Injectable"~ "Injectable",   
                                Method=="IUD"   ~ "IUD",  
                                Method=="LAM"  ~ "LAM",
                                Method=="Pill"  ~ "Pill",
                                Method=="OtherTrad"      | Method==   "PerAbstin"   | Method== "Withdrawal"  ~ "Traditional" )) %>%
  group_by(Survey, Month, Method_Group, Freq ) %>%
  summarise(Freq=sum(Freq))

fill_colors <- c( "Condom"="#FF4340",  "Other Modern"="#FFE2AF",    "Sterilization"="#E2F0D9",      "Implants"="#70AD47",    "Injectable"="#242071", "IUD" ="#CEF9EC",       "LAM"="#20B1FE",             "Pill" ="#FFB636",      "Traditional" ="#C7BEFF" )

           
           
ggplot(subset(results_clean, Survey=="BD2017DHS"), aes(x=Month, y=Freq*100, fill=Method_Group)) +
  annotate('rect', xmin=.5, xmax=1.5, ymin=0, ymax=100, alpha=.2, fill='blue')  +  
  annotate('rect', xmin=1.5, xmax=2.5, ymin=0, ymax=100, alpha=.2, fill='green')  +
  annotate('rect', xmin=2.5, xmax=12.5, ymin=0, ymax=100, alpha=.2, fill='yellow')  +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=fill_colors) +
  scale_x_continuous(breaks=seq(1, 12, 1)) + 
  labs(title="Burkina Faso, 2017", y="PPFP by Month", x="", fill="") +
  theme_bw() +
  theme(    legend.text = element_text(size=18),
    title=element_text(size=18),
    axis.text = element_text(size=18))


for (row in 1:nrow(surveys)) {
  SurveyID <- surveys[row, "API_ID"]
  Country <- surveys[row, "Country"]
  Year <- surveys[row, "FullYear"]
  
  title_id <- paste(Country, " DHS, ", Year, sep="")
  
  ggplot(subset(results_clean, Survey==SurveyID), aes(x=Month, y=Freq*100, fill=Method_Group)) +
    annotate('rect', xmin=.5, xmax=1.5, ymin=0, ymax=100, alpha=.2, fill='blue')  +  
    annotate('rect', xmin=1.5, xmax=2.5, ymin=0, ymax=100, alpha=.2, fill='green')  +
    annotate('rect', xmin=2.5, xmax=12.5, ymin=0, ymax=100, alpha=.2, fill='yellow')  +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=fill_colors) +
    scale_x_continuous(breaks=seq(1, 12, 1)) + 
    labs(title=title_id, y="PPFP by Month", x="", fill="") +
    theme_bw() +
    theme(    legend.text = element_text(size=18),
              title=element_text(size=18),
              axis.text = element_text(size=18))
  
  ggsave(paste("C:/Users/KristinBietsch/files/Track20/FP2020 Results/PPFP Request 2023/PPFP By Month and Method_", Country, Year, ".png", sep=""), height=8, width = 10, units="in")
  
}
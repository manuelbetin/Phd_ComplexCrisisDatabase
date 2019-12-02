

##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

##install common packages
library(SetUpProject) #make sure you have the library

packages <- c("dplyr",
              "ggplot2",
              "plotly",
              "pdftools",
              "lubridate",
              'tictoc',
              "rio",
              "tidytext",
              "stringr",
              "stringi",
              "tidyr",
              "TextMiningCrisis") #make sure you have the library 

## load common packages
load.my.packages(packages)

##### *********************************************************************************************#####
# Import files
##### *********************************************************************************************#####

df1 <- rio::import("files/IMF_urls_clean/Final_links_clean.RData")
df2 <- rio::import("files/IMF_programs_dates.xlsx")

##### *********************************************************************************************#####
# merge tables to match requests with documents
##### *********************************************************************************************#####

df2 <- df2 %>%
  rename(startdate = Period) %>%
  rename(enddate = Date_Expiration) %>%
  rename(ID = ISO3_Code) 

df2=df2 %>%
  mutate(startdate_ext=as.Date(paste(lubridate::year(startdate)-1,lubridate::month(startdate),lubridate::day(startdate),sep="-"),format="%Y-%m-%d"),
         enddate_ext=as.Date(paste(lubridate::year(enddate)+1,lubridate::month(enddate),lubridate::day(enddate),sep="-"),format="%Y-%m-%d"))

df1 <- df1 %>%
  rename(Loss_Date = date) %>%
  #rename(perf_crit = Performance_criteria) %>%
  rename(perf_criterct = performance_criteria) %>%
  rename(ID = iso3c)

# Joining data

output <- full_join(df1, df2, by="ID") %>% 
  filter(Loss_Date >= startdate_ext & Loss_Date <= enddate_ext)
#Keeping those who don't match
liste <- output$reference

#
residual <- df1 %>% dplyr::filter(!reference %in% liste)

c=mydata_init(countries = "ARG",1958,2004,frequency = "month")

##### *********************************************************************************************#####
#  descriptive stats on merge docs
##### *********************************************************************************************#####

N_docs_by_programs=output %>% filter(type_hierarchy=="b_Clean") %>%
  group_by(file) %>% summarize(n_docs=n(),
                               type_docs=paste(unique(paste0(type_program,'(',type_doc,")")),collapse=","))

##### *********************************************************************************************#####
#  export clean database
##### *********************************************************************************************#####



##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
root_path=dirname(current_path)

##install common packages
library("devtools") #make sure you have the library
#install_github("manuelbetin/SetUpProject",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
#install_github("manuelbetin/TextMiningCrisis",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")

packages <- c("dplyr"
              ,"ggplot2"
              ,"plotly"
              ,"pdftools"
              ,"lubridate"
              ,'tictoc'
              ,"rio"
              ,"tidytext"
              ,"stringr"
              ,"stringi"
              ,"tidyr"
              ,"TextMiningCrisis"
              ,"SetUpProject",
              "plotly",
              "shiny"
)

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## INSTRUCTIONS ##########

# Description:
# The script builds a dataframe with the created tf-idf and the Reinhart & Rogoff definition.

#***************************************************************************************####

output=list()
output[["Session_info"]]=sessionInfo()


# Average value of tf-idf per year:

annual_tf_idf <- import("../Betin_Collodel/2. Text mining IMF_data/output/tagged docs/Output_Run_Text_mining.RData")[["tf_idf_table"]] %>% 
  group_by(ISO3_Code,year) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

# Other variables (also standard indicators):

rr <- import("../Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData") %>%
    select(ISO3_Code, year, CC.RR, SD_E.RR, SD_D.RR, BC.LV, CC.LV, SD.LV) %>% # banking crises RR? 
    group_by(ISO3_Code, year) %>% 
    mutate(SD.RR = case_when(SD_E.RR == 1 | SD_D.RR ==1 ~ 1,
                             TRUE ~ 0)) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    arrange(ISO3_Code,year)

# Working dataframe:

output[["comparison_dataframe"]] <- merge(annual_tf_idf, rr, by= c("ISO3_Code","year"), all.x = TRUE) %>% # only countries for which global mining was performed.
  select(-Minutes, -Working_papers, -Issues_papers, -Press_releases) %>% # intermediate indexes to correct problems!
  gather("type_index","value",Deregulation:Track_record) %>%
  gather("type_crisis","dummy_crisis",CC.RR:SD.RR) %>%
  mutate(type_crisis = case_when(str_detect(type_crisis, "CC.RR") ~ "Currency Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "SD.RR") ~ "Sovereign Debt Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "SD_E.RR") ~ "External Sovereign Debt Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "SD_D.RR") ~ "Domestic Sovereign Debt Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "BC.LV") ~ "Banking Crisis-Laeven & Valencia",
                                    str_detect(type_crisis, "CC.LV") ~ "Currency Crisis-Laeven & Valencia",
                                    str_detect(type_crisis, "SD.LV") ~ "Sovereign Debt Crisis-Laeven & Valencia")) %>% 
  separate(type_crisis, c("type_crisis", "database"), sep = "-" ) %>% 
  arrange(ISO3_Code, year, type_index)

# not confuse with output in the server function shiny:

comparison_dataframe <- output[["comparison_dataframe"]]


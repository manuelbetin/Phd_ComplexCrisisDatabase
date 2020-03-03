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
              "shinythemes",
              "purrr"
)

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## INSTRUCTIONS ##########

# Description:
# The script builds a dataframe with the created tf-idf and other databases of crisis.
# It sources the modules for the different tabs of the shiny app and executes it.

#***************************************************************************************####

output=list()
output[["Session_info"]]=sessionInfo()

# Build working dataframe: ----

# Average value of tf-idf per year:

annual_tf_idf <- import("../Betin_Collodel/2. Text mining IMF_data/output/tagged docs/Output_Run_Text_mining.RData")[["tf_idf_table"]] %>% 
  group_by(ISO3_Code,year) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

# Other variables (also standard indicators):

list_vars_comparison <- c("CC.RR","SD_E.RR","SD_D.RR","SD.RR","BC.LV","CC.LV","SD.LV")
  
rr <- import("../Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData") %>% 
    select(ISO3_Code, year, CC.RR, SD_E.RR, SD_D.RR, BC.LV, CC.LV, SD.LV) %>% # banking crises RR?
    # Add general default (domestic + external)
    mutate(SD.RR = case_when(SD_E.RR == 1 | SD_D.RR ==1 ~ 1,
                             TRUE ~ 0)) %>%
    # Laeven & Valencia 0.33 when crisis, not 1: correct
    mutate(BC.LV = case_when(BC.LV > 0 ~ 1,
                             TRUE ~ BC.LV)) %>% 
    mutate(CC.LV = case_when(CC.LV > 0 ~ 1,
                           TRUE ~ CC.LV)) %>%
    mutate(SD.LV = case_when(SD.LV > 0 ~ 1,
                             TRUE ~ SD.LV)) %>%
    # From quarterly to annual:
    arrange(ISO3_Code,year) %>% 
    group_by(ISO3_Code, year) %>% 
    summarise_at(vars(list_vars_comparison), mean, na.rm = TRUE) %>%
    mutate_at(vars(list_vars_comparison), ~ ifelse(.> 0 ,1,.)) 

  

# Working dataframe:

output[["comparison_dataframe"]] <- merge(annual_tf_idf, rr, by= c("ISO3_Code","year"), all.x = TRUE) %>% # only countries for which global mining was performed.
  select(-Minutes, -Working_papers, -Issues_papers, -Press_releases) %>% # intermediate indexes to correct problems!
  gather("type_index","value",Deregulation:Track_record) %>%
  gather("type_crisis","dummy_crisis",CC.RR:SD.LV) %>%
  mutate(type_crisis = case_when(str_detect(type_crisis, "CC.RR") ~ "Currency Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "SD.RR") ~ "Sovereign Debt Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "SD_E.RR") ~ "External Sovereign Debt Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "SD_D.RR") ~ "Domestic Sovereign Debt Crisis-Reinhart & Rogoff",
                                    str_detect(type_crisis, "BC.LV") ~ "Banking Crisis-Laeven & Valencia",
                                    str_detect(type_crisis, "CC.LV") ~ "Currency Crisis-Laeven & Valencia",
                                    str_detect(type_crisis, "SD.LV") ~ "Sovereign Debt Crisis-Laeven & Valencia")) %>% 
  separate(type_crisis, c("type_crisis", "database"), sep = "-" ) %>% 
  arrange(ISO3_Code, year, type_index)


# Problem in the merge when missing years for IMF documents e.g. Thailand 1984.

# Working dataframe for the share graph: -----


share_countries <- output[["comparison_dataframe"]] %>% 
  # Remove duplicates of same index for each country and year
  select(ISO3_Code, year, type_index, value) %>% 
  group_by(ISO3_Code, year) %>% 
  filter(!duplicated(type_index)) %>% 
  ungroup() %>% 
  # Set variable equal to 1 if value of index above 0.
  mutate(occurence = case_when(value > 0 ~ 1,
                               TRUE ~ 0)) %>% 
  select(-value)

# Add to the working dataframe:

output[["comparison_dataframe"]] <-output[["comparison_dataframe"]] %>% 
  merge(share_countries, by= c("ISO3_Code","year","type_index"))

# Not confuse with output in the server function shiny: -----

comparison_dataframe <- output[["comparison_dataframe"]]



# Source modules ----

# Call invisibly all files that start with 5.1

list.files(getwd()) %>%
  str_subset("5\\.1") %>%
  walk(~ source(.x)) 

 # Executes app ----

runApp("5.2 Comparison_app.R")



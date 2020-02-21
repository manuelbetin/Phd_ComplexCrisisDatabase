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
              , "TextMiningCrisis"
              , "SetUpProject"
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
    filter(str_detect(Period, "-01-")) %>% 
    select(ISO3_Code, year, CC.RR_first, SD.RR_first) #banking crises?


# Working dataframe:

output[["comparison_dataframe"]] <- merge(annual_tf_idf, rr, by= c("ISO3_Code","year"), all = TRUE) %>% 
  select(-Minutes, -Working_papers, -Issues_papers, -Press_releases) %>% 
  gather("type_index","value",Deregulation:Track_record)





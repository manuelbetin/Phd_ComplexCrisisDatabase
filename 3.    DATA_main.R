#' @title source script of datapreparation
#' @description consolidate the datasets after
#' performing the textmining. Given the tfidf computed
#' in the TEXTMINING part those scripts merge the 
#' results with the metadata and with alternative
#' macrodata. Also perform the descriptive statistics
#' of the data available.
#'  Codes underlying the section II A quantitative measure
#'  of expert judgement
#'  @author Manuel Betin, Umberto Collodel
#'  @return dataset with final data (tf_idf_database), 
#'  summary tables and figures 
#'  

##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = here::here()
setwd(current_path)
root_path=current_path
##install common packages
library("devtools") #make sure you have the library
install_github("manuelbetin/SetUpProject",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
install_github("manuelbetin/TextMiningCrisis",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
install_github("manuelbetin/PICindex",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")

packages <- c("dplyr","ggplot2","plotly","pdftools","lubridate",'tictoc'
              ,"rio","tidytext","stringr","stringi","tidyr","network","GGally"
              ,"forcats", "TextMiningCrisis", "SetUpProject", "PICindex","gridExtra"
)

## load common packages
SetUpProject::load.my.packages(packages)

source("3.1. DATA_PREPARATION_clean_database.R")

source("3.2. DATA_PREPARATION_get_other_macro_variables.R")

source("3.3. DATA_PREPARATION_check_attrition.R")

source("3.4. DATA_DESCRIPTION_table_corpus_country.R")

source("3.5. DATA_DESCRIPTION_table_detail_corpus.R")

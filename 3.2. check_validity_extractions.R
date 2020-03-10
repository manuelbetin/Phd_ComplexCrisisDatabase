##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = getwd()# rstudioapi::getActiveDocumentContext()$path
root_path=current_path

#source("functions/SetUpProjet.r")
#source("functions/TextMiningCrisis.r")

##install common packages
library("devtools") #make sure you have the library
github_token=rio::import("/Users/manubetin/Dropbox/Manuel/Professionnel/github_token/github_token.txt")
#install_github("manuelbetin/SetUpProject",auth_token=github_token[[1]])
#install_github("manuelbetin/TextMiningCrisis",auth_token=github_token[[1]])

packages <- c("dplyr"
              , "ggplot2"
              , "plotly"
              , "pdftools"
              , "lubridate"
              , 'tictoc'
              ,  "rio"
              , "tidytext"
              , "stringr"
              , "stringi"
              , "tidyr"
              , "rvest"
              , "TextMiningCrisis"
              , "SetUpProject")

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


check_extract=function(path_urls="../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_Requests_Reviews_articleIV.RData",
                       path_tf_idf="../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData",
                       path_final_tf_idf="../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData"){
  
  
  #'check the validity of each steps in the scripts to see what documents are lost
  #'in each step of the process from the download, to the 
  #'text mining and finally to the merging with the rest of the variables
  
  #'@param path_urls the path to locate the dataframe of urls (output of 1. Consolidate_urls.R)
  #'@param path_tf_idf: path to locate the tf_idf matrix (output of 2. Run_text_mining.R)
  #'@param path_final_tf_idf: path to locate the final database containing the tf of the text mining and the
  #' rest of the variables merge after (output of 3. Clean_database.R)
  #'@author Manuel Betin
  #'@return a list with a table given the proportion of the files that have not been properly computed by country
  #' the files that have an error and have not been downloaded and the files that have not been downloaded and
  #' merged
  #' 
  #'@export
  
  metadata=rio::import(path_urls) %>%
    mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs)) 
  vars_metadata=names(metadata)
  size_metadata=dim(metadata)
  name_file_metadata=metadata$name_file %>% unique()
  
  #tf_matrix after running the text mining
  tf=rio::import(path_tf_idf)
  vars_tf=names(tf) 
  size_tf=dim(tf)
  name_file_tf=tf$file %>% unique()
  
  #tf_idf matrix combining all the available data
  final_data=rio::import(path_final_tf_idf)
  vars_final_data=names(final_data) 
  size_final_data=dim(final_data)
  name_file_final_data=final_data$file %>% unique()
  
  lost_files_during_textmining=setdiff(name_file_metadata,name_file_tf)
  missing_urls=metadata %>% filter(name_file %in% lost_files_during_textmining)
  
  error_in_url= missing_urls %>% filter(ID %in% tf$ISO3_Code)
  #proportion of documents properly downloaded
  prop_error_in_url=metadata %>%
    mutate(missing_urls=ifelse(name_file %in% lost_files_during_textmining,1,0)) %>% group_by(ID) %>% mutate(N_tot_docs=n())%>%ungroup()%>%
    group_by(ID,missing_urls) %>% summarize(Prop_missing_urls=1-n()/first(N_tot_docs)) %>% filter(missing_urls==0) %>% ungroup() %>% dplyr::select(-missing_urls)
  
  return(list(proportion_error_in_url=prop_error_in_url,
              files_with_error_in_url=error_in_url,
              files_not_downloaded=missing_urls))
}

my_validity_check=check_extract()
print(my_validity_check$proportion_error_in_url)





##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

##install common packages
library("SetUpProject") #make sure you have the library

packages <- c("dplyr",
              'tictoc',
              "rio",
              "tidytext",
              "stringr",
              "stringi",
              "tidyr",
              'crayon',
              "TextMiningCrisis") #make sure you have the library 

## load common packages
load.my.packages(packages)

#--------------------
download_pdfs=T

#---------------
clean_IMF_urls=function(file){
  EBS_files=url_links %>% filter(str_detect(hierarchy,"EBS"))
  EBS_files_cor=EBS_files %>% filter(str_detect(hierarchy,"Cor")) %>% mutate(type_hierarchy="Correction")
  EBS_files_sup=EBS_files %>% filter(str_detect(hierarchy,"Sup")) %>% mutate(type_hierarchy="Supplement")
  EBS_files_clean=EBS_files %>% filter(!(str_detect(hierarchy,"Sup") | str_detect(hierarchy,"Cor"))) %>% mutate(type_hierarchy="Clean")
  final_dt=rbind(EBS_files_clean,EBS_files_cor,EBS_files_sup)
  final_dt=final_dt %>% mutate(date=as.Date(date,format="%b %d %Y"))
  final_dt
}

find_keyword_list=function(files){
  keywords=paste0(files$keywords,collapse=", ")
  
  #keywords=clean_text(keywords)
  keywords=strsplit(keywords, ",")[[1]]
  
  keywords=data.frame(keywords)
  colnames(keywords)="keywords"
  
  summary_keywords=keywords %>% group_by(keywords) %>% summarize(n=n()) %>% arrange(-n) %>% mutate(keywords=str_replace_all(keywords,"\\[",''),
                                                                                  keywords=str_replace_all(keywords,"\\]",''),
                                                                                  keywords=str_replace_all(keywords,"\\'",''),
                                                                                  keywords=substr(keywords,2,100))
  return(list(IMF_keywords=summary_keywords$keywords,summary_keywords=summary_keywords))
  
}

name_links_dt="IMF_urls_clean/IMFECF_Requests_links_clean.RData"
url_links=rio::import(paste0("files/",name_links_dt))
url_links=clean_IMF_urls(url_links)

#-------------------
## Download files ##
#------------------

# download EBS alone  ------

urls_clean=url_links %>% filter(type_hierarchy=="Clean")
if(download_pdfs==T){
  pdf_from_url(urls_clean,"files/IMFECF Program requests2")
}

#-------------------
## Aggregate and summary stats ##
#-------------------
name_links_dt="IMFECF_ESAF_Requests_links.csv"
name_output=str_replace(name_links_dt,".Rdata","")
name_output=str_replace(name_output,"links","")
name_output=str_replace_all(name_output,"_"," ")

files=aggregate_corpus("files/IMFECF Program requests")

#number of pages by documents
pdf_page_count(files)

#find occurence of word in document
eval_pages(files,c("Corrections"),brute_freq = T)




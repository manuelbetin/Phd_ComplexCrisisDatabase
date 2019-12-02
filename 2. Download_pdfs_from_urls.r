
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


name_links_dt="IMF_urls_clean/Final_links_clean.RData"
url_links=rio::import(paste0("files/",name_links_dt))
head(url_links)

url_links=url_links %>% mutate(name_file=paste0(file,"_",Loss_Date,"_",type_doc,"_RN",Review_number,"_",str_replace_all(hierarchy,"/","_")))

#-------------------
## Download files ##
#------------------

# download EBS alone  ------

url_links=url_links %>% filter(type_hierarchy=="b_Clean")
#url_links=url_links %>% filter(ID %in% c("URY","BRA","COL"))
if(download_pdfs==T){
  pdf_from_url(url_links,"files/All programs")
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
#eval_pages(files,c("Corrections"),brute_freq = T)




##install common packages
library("devtools") #make sure you have the library
github_token=rio::import("/Users/manubetin/Dropbox/Manuel/Professionnel/github_token/github_token.txt")

#install_github("manuelbetin/SetUpProject",auth_token=github_token[[1]])
install_github("manuelbetin/TextMiningCrisis",auth_token=github_token[[1]])

packages <- c("dplyr"
              , 'tictoc'
              , "rio"
              , "tidytext"
              , "stringr"
              , "stringi"
              , "tidyr"
              , "ggplot2"
              , "lubridate"
              , 'crayon'
              , "DT"
              , "plotly"
              , "TextMiningCrisis"
              , "SetUpProject"
              , "pdftools"
              , "purrr"
              , "rvest")


## load common packages
SetUpProject::load.my.packages(packages)

#--------------------------------------------

#' @title download the urls of the most recent IMF publication
#' @aliases consolidate_recent_urls
#' @description apply the scrapping of the IMF website to get the 
#' urls of the recently published documents
#' @author Manuel Betin
#' @return a dataset containing the title, country, date and metadata of the 
#' recently published documents

#--------------------------------------------

#IMF_links=get_imf_country_reports()
#rio::export(IMF_links,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/recent_IMF_urls.RData")

mydt=get_imf_country_reports(npages=3)
#run_tf_by_chunk(mydt,keyword_list = names(lexicon()),ENGINE=pdf_text,delete_pdfs = F)
#get_sentences(corpus$ITAEA2020001,c("Official_support"))

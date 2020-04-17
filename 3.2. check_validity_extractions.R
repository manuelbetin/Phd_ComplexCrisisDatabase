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
              , "stargazer"
              , "xtable"
              , "TextMiningCrisis"
              , "SetUpProject")

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

my_validity_check=check_extract()
print(my_validity_check$proportion_error_in_url)

stargazer(my_validity_check$proportion_error_in_url
          , title="NPL Attrition"
          , type="latex"
          , digits=2
          , no.space=T
          , align=T
          , summary=F
          , rownames=F
          , table.placement = "H"
          , column.sep.width="3pt"
          , font.size = "footnotesize"
          , out="../Betin_Collodel/2. Text mining IMF_data/output/extraction_attrition/extract_attrition.tex")


Lexicon_summary=lapply(names(lexicon()),function(x){
  Index=x
  n.words=length(lexicon_details(x)[[1]])
  examples=str_remove_all(paste0(lexicon_details(x)[[1]][1:4],collapse=', '),", NA")
  cbind(Index,n.words,examples)
})
Lexicon_summary=do.call(rbind,Lexicon_summary)
Lexicon_summary=data.frame(Lexicon_summary)
Lexicon_summary=Lexicon_summary %>% mutate(n.words=as.numeric(n.words),
                           examples=paste0(examples,",...")) %>% arrange(-n.words) 

print(xtable(Lexicon_summary
       , caption="Summary Lexicon"
       , type="latex"
       , digits=0
       , no.space=T
       , align=c("l","l","l","l")
       , summary=F
       , rownames=F
       , table.placement = "H"
       , column.sep.width="3pt"
       , font.size = "footnotesize")
       , file="../Betin_Collodel/2. Text mining IMF_data/output/Lexicon/Lexicon_summary.tex")


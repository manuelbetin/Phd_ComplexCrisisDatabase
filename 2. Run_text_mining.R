

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
              , "TextMiningCrisis"
              , "SetUpProject")

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## SECTION OF INSTRUCTIONS AND PARAMETERS ##########

## Description:
## The script takes as input a corpus of pdf located in a directory and 
## perform a text analysis  using a lexicon of economic crisis to 
## observe the type of crisis that the text mention

## Instructions:
## replace 1) and 2)

##Remove comment to use all available word groups
#keyword_list=names(key_words_crisis())

##Remove comment to next line to use all available categories
#keyword_list=names(key_words_categories())

##Manual selection
keyword_list=c('Currency_crisis',"Balance_payment_crisis")#,'Severe_recession',"Banking_crisis")

# keyword_list=c('Reform_agenda','Political_crisis','Balance_payment_crisis','World_outcomes',
#                'Contagion','Expectations','Currency_crisis',
#                'Financial_crisis','Severe_recession','Soft_recession',
#                'Fiscal_outcomes','Banking_crisis','Commodity_crisis','Inflation_crisis',
#                'Sovereign_default','Fiscal_consolidation')

#***************************************************************************************####
output=list()
output[["Session_info"]]=sessionInfo()

#***************************************************************************************####

name_links_dt="urls_Requests_Reviews_articleIV.RData" #name of the dataframe containing the urls
url_links=rio::import(paste0("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/",name_links_dt)) %>%
  mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs)) %>%
  filter(str_detect(pdf,".pdf"))

#filter(name_file %in% sample(url_links$name_file,15))

my_urls=list(url_links %>% filter(ID=="USA")
             #, url_links %>% filter(ID=="ARG")
            #, url_links %>% filter(ID=="URY")
             )

#run the tf chunk by chunk
lapply(1:length(my_urls),function(x){
  ID=my_urls[[x]]$ID %>% unique() 
  run_tf_by_chunk(data.frame(my_urls[[x]]),keyword_list,ID,delete_pdfs = T)
})

#consolidate into a single database
mytfs=list.files("temp/tf",full.names = T)
mytfs=lapply(mytfs,function(x){
  y=rio::import(x)
  data.frame(y)
  })
mytfs=do.call(rbind,mytfs)

#extract from the names of the files the country, date and hierarchy of the document
dt=mytfs %>% mutate(#year=substr(file,5,8),
  ISO3_Code=substr(file,1,3),
  Period=as.Date(str_match(mytfs$file,"\\d\\d\\d\\d-\\d\\d-\\d\\d")),
  type_doc=substr(file,str_length(ISO3_Code)+str_length(Period)+3,str_length(file)))

## idf ----------
LoI_idf=idf(dt)

myidf_plot=idf_barplot(LoI_idf,vars_type=c("economic_shock","debt_outcomes","non_economic_shock","adjustment_program"),idf_trans = T)
myidf_plot$fig+ggsave("2.graphs/tagged docs/tf-idf/idf.png")

## tf-idf ----------
### tf-idf table ------

LoI_tf_idf=tf_idf(dt,weight_method="brut_frequency")

output[["tf_idf_table"]]=LoI_tf_idf
rio::export(LoI_tf_idf,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")

### tf-idf by type of crisis ------------------

mytf_plot=tf_barplot(dt,vars_type=c("economic_shock","non_economic_shock","debt_outcomes"))

mytf_plot$tf_fig_avg+ggsave("2.graphs/tagged docs/tf-idf/tf_avg.png")

mytf_plot$tf_fig_avg_prop+ggsave("2.graphs/tagged docs/tf-idf/tf_avg_prop.png")

# Cos sim Crisis -----------------------------

LoI_cos_sim=cosim_matrix(LoI_tf_idf)
output[["cos_sim_table_crisis"]]=LoI_cos_sim
rio::export(LoI_cos_sim,"2.data/tagged docs/cos_sim.RData")
LoI_cos_sim=data.frame(LoI_cos_sim)
LoI_cos_sim$Crisis=rownames(LoI_cos_sim)
select_cols=names(LoI_cos_sim)[!names(LoI_cos_sim) %in% c("Crisis")] 

output[["cos_sim_fig_crisis"]]=lapply(select_cols,function(x){
  plot_cos_sim(LoI_cos_sim,x)+
    ggsave(paste0("2.graphs/tagged docs/cos_sim/cos_sim_",x,".png"))
})

#Final output -----

final_destination="../Betin_Collodel/2. Text mining IMF_data/output/tagged docs/Output_Run_Text_mining.RData"
save(output,file=final_destination)

print(paste0("All the Output of the script has been saved in the following directory:"))
print(final_destination)

#-------------------------------------------

# dt2=dt %>% mutate(yearqrt=paste0(year(Period),"_",quarter(Period))) %>%
#   group_by(ISO3_Code,yearqrt) %>% summarize(Soft_recession=mean(Soft_recession,na.rm=T),
#                                             Fiscal_outcomes=mean(Fiscal_outcomes,na.rm=T)) %>% ungroup()
 ggplot(dt %>% filter(ISO3_Code=="USA"))+
   geom_line(aes(x=Period,y=Balance_payment_crisis,color=ISO3_Code))+
   scale_x_date(date_breaks = "2 year",date_labels =  "%Y")+
   theme_bw()+
   theme(axis.text.x=element_text(angle=90,hjust=1),legend.position = "bottom",axis.text=element_text(size=12))




##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = here::here()# rstudioapi::getActiveDocumentContext()$path
root_path=current_path

#source("functions/SetUpProjet.r")
#source("functions/TextMiningCrisis.r")

## Install packages:

library("devtools") #make sure you have the library
library("purrr")
library("dplyr")
library("tidyverse")
library("pdftools")
library("lubridate")
library("tictoc")
library("rio")
library("tidytext")
library("stringr")
library("stringi")
library("rvest")
library("tidyr")
library("crayon")



# Load all packages from TextMiningCrisis:

list.files("/Users/Umberto/Desktop/TextMiningCrisis-master/R") %>% 
  map(~ paste0("/Users/Umberto/Desktop/TextMiningCrisis-master/R/", .x, sep = "")) %>% 
  map(source)



#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## SECTION OF INSTRUCTIONS AND PARAMETERS ##########

## Description:
## The script takes as input a dataframe containing the urls where to download the 
## documents and perform a text analysis  using a lexicon of economic crisis to 
## observe the type of crisis that the text mention

## Instructions:
## replace 1) and 2)
apply_tf_on_new_ctry=T

usb_drive="/Volumes/Elements/IMF documents"
delete_pdfs=F
rm_short_docs=T
min_words=500
##Remove comment to use all available word groups
#keyword_list=names(key_words_crisis())

##Remove comment to next line to use all available categories
keyword_list=names(key_words_crisis())

##Manual selection
<<<<<<< HEAD
#keyword_list=c('Currency_crisis',"Balance_payment_crisis")#,'Severe_recession',"Banking_crisis")
=======
keyword_list=c('Currency_crisis',"Currency_crisis_severe")#,'Severe_recession',"Banking_crisis")
>>>>>>> 8692d4d7e75c85cd22b88546c6f1a886f0532ced

# keyword_list=c('Reform_agenda','Political_crisis','Balance_payment_crisis','World_outcomes',
#                 'Contagion','Expectations','Currency_crisis',
#                 'Financial_crisis','Severe_recession','Soft_recession',
#                 'Fiscal_outcomes','Banking_crisis','Commodity_crisis','Inflation_crisis',
#                 'Sovereign_default','Fiscal_consolidation')

#***************************************************************************************####
output=list()
output[["Session_info"]]=sessionInfo()

#***************************************************************************************####

correct_url=function(link){
  #ad hoc function that to correct the links for the IMF url when url does not refer to the pdf document
  path_file=link[[1]]
  html_file <- try(read_html(path_file), silent=T)
  if("try-error" %in% class(html_file)) {
    warning(paste(path_file,": Error in path file",sep=""))
    content=NA
  }else {
    content=try({
      content<- html_file %>% html_nodes(xpath=  '//*[@id="ais-detail-container"]/div[2]/div/ul') %>%html_nodes("a") %>% html_attr("href")
      content[(str_detect(content,"pdf")|str_detect(content,"PDF"))]
      })
    if("try-error" %in% class(content)) {
      warning(paste(path_file,": Error in path file",sep=""))
      content=NA
    }else{
      return(content)
    }
    #content=data.frame(content)
  }
  
}

ctries=c("ARG")

if(apply_tf_on_new_ctry==T){
  
  #run the tf chunk by chunk, it will create a temp folder with the corpus made out of the
  #documents selected and the tf computed.
  #when documents have not a valid url it will find the correct one on the IMF website

  lapply(ctries,function(x){
  path_external_usb=paste0(usb_drive,"/",x)
  #Load the database of urls output of the script 1. consolidate_urls.R
  data("IMF_docs_urls")
  url_links=IMF_docs_urls %>%
    mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs)) %>% filter(ID==x)
  #filter(str_detect(pdf,".pdf") | str_detect(pdf,".PDF"))

  # correct incorrect urls by rescarping the imf website to get the link to the pdf
  if(!file.exists(paste0(path_external_usb,"/updated urls/updated_urls_",x,".RData"))){
  for(i in 1:dim(url_links)[1]){
    if(!(str_detect(url_links[i,"pdf"],".pdf") | str_detect(url_links[i,"pdf"],".PDF"))){
      print(url_links[[i,"name_file"]])
      url_links[i,"pdf"]=correct_url(url_links[i,"pdf"])
    }
  }
  
  dir.create(paste0(path_external_usb,"/updated urls/"))
  rio::export(url_links,paste0(path_external_usb,"/updated urls/updated_urls_",x,".RData"))
  }else{
  url_links=rio::import(paste0(path_external_usb,"/updated urls/updated_urls_",x,".RData"))
  }
  run_tf_by_chunk(urls=url_links,
                    keyword_list=keyword_list,
                    extract_number=x,
                    delete_pdfs = delete_pdfs,
                    rm_short_docs=rm_short_docs,
                    min_words=100,loc_temp =path_external_usb)
  })
}

#consolidate into a single database the tf matrix of all countries

mytfs=setdiff(dir(usb_drive),c("download_docs.r","urls_Requests_Reviews_articleIV.RData","0. logs","0. Old extraction","tf_idf.RData"))

mytfs=lapply(mytfs,function(x){
  if(dir.exists(paste0(usb_drive,"/",x,"/tf"))){
    y=rio::import(paste0(usb_drive,"/",x,"/tf/tf_crisis_words_",x,".RData"))
    data.frame(y)
  }
  })
mytfs=do.call(rbind,mytfs)

if(mytfs %>% map(length) %>% reduce(`==`) == FALSE){
  # Check that tf indexes have same number of columns
  mytfs_different_length <- mytfs %>% map(length) %>% unique()
  stop("Dataframes with tf indexes do not have the same length: ", paste(mytfs_different_length, collapse = " and ")) # 
  } else {
    # If TRUE, check that col. names are equal. 
    if(mytfs %>% map(names) %>% reduce(`==`) == rep(F,mytfs %>% map_int(ncol) %>% unique())) {
    stop("Dataframes with tf indexes do not have same column names.")
    } else {
      
      mytfs <- do.call(rbind,mytfs)
      
    }
  }

# Extract from the names of the files, the country, date and hierarchy of the document.

dt <- mytfs %>% mutate(
  ISO3_Code=substr(file,1,3),
  year=substr(file,5,8),
  Period=as.Date(str_match(mytfs$file,"\\d{4}-\\d{2}-\\d{2}")),
  type_doc=substr(file,str_length(ISO3_Code)+str_length(Period)+3,str_length(file)))

## Compute the idf ----------

LoI_idf=idf(dt)

myidf_plot=idf_barplot(LoI_idf,vars_type=c("economic_shock","debt_outcomes","non_economic_shock","adjustment_program"),idf_trans = T)
myidf_plot$fig+ggsave("2.graphs/tagged docs/tf-idf/idf.png")

## compute the tf-idf ----------
### tf-idf table ------

LoI_tf_idf=tf_idf(dt,weight_method="brut_frequency")

#export the tf_idf
output[["tf_idf_table"]]=LoI_tf_idf
rio::export(LoI_tf_idf,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")
rio::export(LoI_tf_idf,paste0(usb_drive,"/tf_idf.RData"))


### tf-idf by type of crisis ------------------

mytf_plot=tf_barplot(dt,vars_type=c("economic_shock","non_economic_shock","debt_outcomes"))

mytf_plot$tf_fig_avg+ggsave("2.graphs/tagged docs/tf-idf/tf_avg.png")

mytf_plot$tf_fig_avg_prop+ggsave("2.graphs/tagged docs/tf-idf/tf_avg_prop.png")

# Compute the Cosinus similarity of each Crisis -----------------------------

LoI_cos_sim=cosim_matrix(LoI_tf_idf)
output[["cos_sim_table_crisis"]]=LoI_cos_sim
#export the cosinus similariry
rio::export(LoI_cos_sim,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/cos_sim.RData")
LoI_cos_sim=data.frame(LoI_cos_sim)
LoI_cos_sim$Crisis=rownames(LoI_cos_sim)
select_cols=names(LoI_cos_sim)[!names(LoI_cos_sim) %in% c("Crisis")] 

output[["cos_sim_fig_crisis"]]=lapply(select_cols,function(x){
  plot_cos_sim(LoI_cos_sim,x)+
    ggsave(paste0("2.graphs/tagged docs/cos_sim/cos_sim_",x,".png"))
})

#Export the final output -----
final_destination="../Betin_Collodel/2. Text mining IMF_data/output/tagged docs/Output_Run_Text_mining.RData"
save(output,file=final_destination)

print(paste0("All the Output of the script has been saved in the following directory:"))
print(final_destination)

#-------------------------------------------


?substr

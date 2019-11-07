##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

##install common packages
library(SetUpProject) #make sure you have the library

packages <- c("dplyr",
              "ggplot2",
              "plotly",
              "pdftools",
              "lubridate",
              'tictoc',
              "rio",
              "tidytext",
              "stringr",
              "stringi",
              "tidyr",
              "TextMiningCrisis") #make sure you have the library 

## load common packages
load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## SECTION OF PARAMETERS ##########

# Description:
# The script takes as input a corpus of pdf located in a directory and 
# perform a text analysis  using a lexicon of economic crisis to 
# observe the type of crisis that the text mention

# If you want to rerun the analysis from scratch delete the following folders and files:
# 1.corpus, 2.data,3.graphs, output.RData
# and rerun the code


#instructions:
# replace 1) and 2)
current_path = rstudioapi::getActiveDocumentContext()$path
root_path=dirname(current_path)

#   1)
name_corpus_file="files/IMFECF ESAF Program requests Clean" #name of folder containing the files
files_path=paste0(root_path,"/",name_corpus_file) 

#   2)
#SELECT THE TYPE OF LEXICON OF WORDS
lexicon="words" #"words" or "category" 
 
#Remove comment to use all available word groups
#keyword_list=names(key_words_crisis())

#Remove comment to use all available categories
#keyword_list=names(key_words_categories())

#Manual selection
keyword_list=c('Reform_agenda','Political_crisis','Balance_payment_crisis','World_outcomes',
                 'Contagion','Expectations','Currency_crisis',
                 'Financial_crisis','Severe_recession','Soft_recession',
                'Fiscal_outcomes','Banking_crisis','Commodity_crisis','Inflation_crisis',
                 'Sovereign_default','Fiscal_consolidation')

#***************************************************************************************####


# Create directories for output ----
if(!dir.exists(paste0(root_path,"/",name_corpus_file))){
  print("Please provide file path directory")
}
dir.create(paste0(root_path,"/1.corpus"))
dir.create(paste0(root_path,"/3.graphs"))
dir.create(paste0(root_path,"/3.graphs/radar"))
dir.create(paste0(root_path,"/3.graphs/tf-idf"))
dir.create(paste0(root_path,"/3.graphs/cos_sim"))
data_export=dir.create(paste0(root_path,"/2.data"))

output=list()
output[["Session_info"]]=sessionInfo()

# Aggregate corpus----

corpus=aggregate_corpus(paste0(root_path,"/",name_corpus_file))
save(corpus,file=paste0(root_path,"/1.corpus/corpus_clean.RData"))

# Compute tf, idf, tf-idf ---------------------------
## tf  ----------


if(!file.exists(paste0(root_path,"/2.data/data_LoI_N_Occ_crisis_words.RData"))){
  dt=run_tf(corpus_path=paste0(root_path,"/1.corpus/corpus_clean.RData"),
           type_lexicon = lexicon,
           keyword_list = keyword_list,
           export_path=paste0(root_path,"/2.data"),
           parrallel=T)
}else{dt=rio::import(paste0(root_path,"/2.data/data_LoI_N_Occ_crisis_words.RData"))}

output[["tf_table"]]=dt

dt=dt %>% mutate(year=2019,
                 ISO3_Code="World",
                 Period=as.Date("2019-10-01"),
                 variable="EO")

## idf ----------
LoI_idf=invers_doc_frequency(dt)
output[["idf_table"]]=LoI_idf

LoI_idf_dt=LoI_idf%>% mutate(Crisis=str_replace(Crisis,"_"," "))

output[["idf_fig"]]=ggplot(LoI_idf_dt)+
  geom_bar(stat="identity",aes(x=reorder(Crisis,idf),y=idf))+
  theme_bw()+
  labs(x=NULL)+
  theme(legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=10))+
  ggsave(paste0(root_path,"/3.graphs/tf-idf/idf_",lexicon,".png"))

## tf-idf ----------
### tf-idf table ------
LoI_tf_idf=table_word_weight(dt,weight_method="brut_frequency")
output[["tf_idf_table"]]=LoI_tf_idf
rio::export(LoI_tf_idf,paste0(root_path,"/2.data/tf_idf_",lexicon,".RData"))

#radar charts

dt_year_radar=evol_weight_by_group(LoI_tf_idf)
dt_year_radar=dt_year_radar %>% group_by(Crisis) %>% summarize(word_weight=mean(word_weight,na.rm=T)) %>%
  filter(!Crisis %in% c("year",'Period','variable','ISO3_Code'))
year_radar=country_radar_fig(dt_year_radar)
htmlwidgets::saveWidget(year_radar,paste0(root_path,"/3.graphs/radar/radar_",lexicon,".html"))
output[["tf-idf_fig_radar"]]=year_radar
  
### tf-idf by type of crisis ------------------

most_common_crisis_tf_idf=LoI_tf_idf %>% ungroup() %>% dplyr::select(-c("ISO3_Code","year","Period","variable","file"))%>% summarize_all(mean) %>% 
  mutate(ISO3_Code="All") %>% ungroup() %>% dplyr::select(-ISO3_Code)
most_common_crisis_tf_idf=data.frame(t(most_common_crisis_tf_idf))
most_common_crisis_tf_idf$crisis=rownames(most_common_crisis_tf_idf)
colnames(most_common_crisis_tf_idf)[1]="values"
most_common_crisis_tf_idf=most_common_crisis_tf_idf %>% mutate(values=as.numeric(as.character(values)))

output[["tf_idf_table_avg"]]=most_common_crisis_tf_idf

output[["tf_idf_fig_avg"]]=ggplot(most_common_crisis_tf_idf)+
  geom_bar(stat="identity",aes(x=reorder(crisis,values),y=values))+
  theme_bw()+
  labs(x=NULL,
       y="tf-idf")+
  theme(legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=10))+
  ggsave(paste0(root_path,"/3.graphs/tf-idf/tf_idf_",lexicon,".png"))

# Cos sim Crisis -----------------------------

LoI_cos_sim=matrix_cosinus_similarity(LoI_tf_idf)
output[["cos_sim_table_crisis"]]=LoI_cos_sim
rio::export(LoI_cos_sim,paste0(root_path,"/2.data/cos_sim_",lexicon,".RData"))
LoI_cos_sim=data.frame(LoI_cos_sim)
LoI_cos_sim$Crisis=rownames(LoI_cos_sim)
select_cols=names(LoI_cos_sim)[!names(LoI_cos_sim) %in% c("Crisis")] 

output[["cos_sim_fig_crisis"]]=lapply(select_cols,function(x){
  plot_cos_sim(LoI_cos_sim,x)+
    ggsave(paste0(root_path,"/3.graphs/cos_sim/cos_sim_",x,"_",lexicon,".png"))
})

#Final output -----

final_destination=paste0(root_path,"/Output.RData")
save(output,file=final_destination)

print(paste0("All the Output of the script has been saved in the following directory:"))
print(final_destination)

#-------------------------------------------

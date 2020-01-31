#                          Text Mining for Economic Crisis

***
***

## Description

Text mining on pdf documents. 

### Codes

- **1.1. consolidate_urls.R**: take the full list of urls and subsample only the relevant documents
- **1. Clean_urls_report.rmd:** once the list of urls is ready use this script to clean the urls and generate report
- **2. Run_Text_mining.r** use the dataframe of urls to download pdfs, generate the corpus,and run the text mining on thedocuments
- **3. Clean_database.r** use the dataframe of urls to download pdfs, generate the corpus,and run the text mining on thedocuments

## Author

- Manuel Betin
- Umberto Collodel

## Language

- R 

## Dependencies

### Libraries

- "manuelbetin/SetUpProject" available from github (private access)
- "manuelbetin/TextMiningCrisis" available from github (private access)
- pdftools 
- xml2
- rvest
- tidytext
- tidyr
- stringr
- stringi
- tidytext
- dplyr
- tidyr
- plotly
- ggplot2
- rio
- tictoc
- lubridate

 
## Structure and workflow of code


### 1. consolidate_urls.R

---------

1. **Consolidate all urls by country into a single file**
files=list.files("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_by_ctry")

``` R
[...]

```

2. **Export consolidated urls database**

``` R
rio::export(dt,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/consolidated_urls_by_ctry.RData")
```

3. **Create functions to extract the type of document from the title each functions will be  then use to modify the database of url by including columns when a document belong to a category**

``` R

find_IMFprograms=function(dt)
   
 [..]
   
find_increase_quotas=function(dt)
  
[..]
  
ind_membership=function(dt)

[..]

find_name_from_title=function(dt)

[..]

find_repurchase=function(dt)

[..]

find_consultations=function(dt)

  [..]
  
find_statements=function(dt)
  
  [..]
  
find_technical_assistance=function(dt)
  
  [..]
  
find_board_governors=function(dt)
  
  [..]
  
find_overdue_financial_obligations=function(dt)
  
  [..]
  
find_expost_assesments=function(dt)
  
  [..]
  
find_exchange_system=function(dt)
  
  [..]
  
find_program_type=function(dt)
  
  [..]
  
create_file_name=function(dt)
  
  [..]
```
  
4.  **Apply functions by piping all functions**
``` R
dt=dt %>% 
  find_IMFprograms() %>%
  find_consultations() %>%
  find_name_from_title() %>%
  find_membership() %>%
  find_statements() %>% 
  find_repurchase() %>%
  find_technical_assistance() %>%
  find_expost_assesments()%>%
  find_board_governors() %>%
  find_exchange_system() %>%
  find_overdue_financial_obligations() %>%
  find_program_type() %>%
  create_file_name() %>%
  find_increase_quotas()

[..]
```

5.  **Export separate database of urls for the different types of document**

``` R
 [..]

dt_IMF_consultations=dt %>% 
  filter(type_doc_consultations %in% c("Article IV","Article XIV","Eco developments","consultations","exchange system") | !is.na(type_doc_programs)) %>%
  group_by(iso3_from_title,period,year,type_doc_consultations) %>% 
  summarize_all(funs(first)) %>% 
  ungroup() %>%
  mutate(name_file=paste0(iso3_from_title,"_",period,"_",type_doc_programs))  %>%
  rename(Loss_Date = period) %>%
  #rename(perf_crit = Performance_criteria) %>%
  rename(perf_criteria = performance_criteria) %>%
  rename(ID = iso3_from_title)

rio::export(dt_IMF_consultations,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_consultations.RData")

[..]
```

6.  **Consolidate old and new extractions to obtain a single database with all documents of interests: crisis periods: requests and reviews**

 non crisis periods: article IV, consultations, recent economic development, article XIV and exchange system.
 this is important because for all requests and review the old extractions has more metadata so we can remove files that correspond to corrections.

``` R
dt=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_consultations.RData")

dt_old=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_extraction_with_hierarchies.RData")

[..]
```

7. **Create a summary of the documents that we keep by country and type of document**

``` R
[...]
```

8. **Export the final database of interest containing consultations, requests and reviews and that will provide data for non crisis and crisis period**

``` R
rio::export(summary_available_documents,"../Betin_Collodel/2. Text mining IMF_data/output/summary available files/summary_N_urls_Requests_Reviews_articleIV.csv")
rio::export(mydt,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_Requests_Reviews_articleIV.RData")
```



### 2. Run_text_mining.R

------------


**Description:**
 
The script takes as input a dataframe containing the urls where to download the  documents and perform a text analysis  using a lexicon of economic crisis to observe the type of crisis that the text mention

1. **Load the database of urls output of the script 1. consolidate_urls.R**
``` R

name_links_dt="urls_Requests_Reviews_articleIV.RData" #name of the dataframe containing the urls
url_links=rio::import(paste0("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/",name_links_dt)) %>%
  mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs)) %>%
  filter(str_detect(pdf,".pdf"))
```

2. **Create chunks of the database with a list containing the urls for each country that you want**

``` R
my_urls=list(url_links %>% filter(ID=="URY") %>% arrange(ID,period)
             #, url_links %>% filter(ID=="ARG")
            #, url_links %>% filter(ID=="URY")
             )
```

3. **Run the tf chunk by chunk, it will create a temp folder with the corpus made out of the documents selected and the tf computed **

``` R
lapply(1:length(my_urls),function(x){
  ID=my_urls[[x]]$ID %>% unique() 
  run_tf_by_chunk(data.frame(my_urls[[x]]),keyword_list,ID,delete_pdfs = F)
})
```

4. **Consolidate into a single database the tf matrix of all countries**

``` R
mytfs=list.files("temp/tf",full.names = T)
mytfs=lapply(mytfs,function(x){
  y=rio::import(x)
  data.frame(y)
  })
mytfs=do.call(rbind,mytfs)
```

5. **Extract from the names of the files the country, date and hierarchy of the document**

``` R
dt=mytfs %>% mutate(#year=substr(file,5,8),
  ISO3_Code=substr(file,1,3),
  Period=as.Date(str_match(mytfs$file,"\\d\\d\\d\\d-\\d\\d-\\d\\d")),
  type_doc=substr(file,str_length(ISO3_Code)+str_length(Period)+3,str_length(file)))
```

6. **Compute the idf**

``` R
LoI_idf=idf(dt)

[..]
```

7. **Compute the tf-idf**

``` R
[..]
```

8. **Export the tf_idf**

``` R
output[["tf_idf_table"]]=LoI_tf_idf
rio::export(LoI_tf_idf,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")
```

9. **tf-idf by type of crisis **

``` R
[..]
```

10. **Compute the Cosinus similarity of each Crisis **

``` R
[..]
```

10. **Export the cosinus similariry**

``` R
rio::export(LoI_cos_sim,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/cos_sim.RData")

[..]
```

11. **Export the final output**

``` R
final_destination="../Betin_Collodel/2. Text mining IMF_data/output/tagged docs/Output_Run_Text_mining.RData"
save(output,file=final_destination)
```
### 3. Clean_database.r

-------

1. **Import term frequency data and merge with metadata from urls **

``` R

LoI_tf_idf=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")
```

2. **Import metadata from url extractions**

``` R
name_links_dt="urls_Requests_Reviews_articleIV.RData" #name of the dataframe containing the urls
dt_meta=rio::import(paste0("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/",name_links_dt)) %>%
  mutate(file=paste0(ID,"_",period,"_",type_doc_programs)) %>%
  filter(str_detect(pdf,".pdf"))
```

3. **Merge datasets with metadata from extraction and clean variable names**

``` R
mydata=LoI_tf_idf %>% left_join(dt_meta,by=c("file")) %>% 
  rename(iso3c=ID) %>% dplyr::select(-c(ISO3_Code,period)) %>%
  dplyr::select(iso3c,Period,file,title,type_doc,type_program,type_doc_programs,type_doc_consultations,perf_criteria,membership,
                statements,repurchase_transaction,technical_assistance,expost_assessment,
                exchange_system,overdue_obligations,Review_number,pdf,type_hierarchy,
                hierarchy,waiver,modification,everything())
```

4. **Create normalization of variables**

``` R
[...]
```

5. **Regroup type documents into lower level categories: request, reviews, cancelations and modifications, consultations and technical assistance**

``` R
mydata=mydata %>% mutate(type=ifelse(modification=="modification" | type_doc %in% c("modification"),"modification",NA),
                         type=ifelse(type_doc %in% c("request"),"request",type),
                         type=ifelse(type_doc %in% c("review","request and review","extension",
                                                     "cancelation and request","performance criteria","waiver",
                                                     "Use fund","purchase transac",
                                                     "request for postponement"),"review",type),
                         type=ifelse(type_doc %in% c("cancelation"),"cancelation",type),
                         type=ifelse(type_doc %in% c("consultations","Eco developments","Article IV","Article XIV","exchange system"),"consultation",type),
                         type=ifelse(type_doc %in% c("request for technical assistance"),"technical assistance",type))
```

6. **For consultations and technical assistance we do not have info on hierarchy so we cannot spot corrections and supplement we will consider that they are all clean docs**

``` R
[..]
```

7.  **Seach for unconfirmed requests**

``` R
mydata =mydata %>% mutate(type=ifelse(type %in% c("request") & is.na(type_hierarchy),"request not confirmed",type),
                          type=ifelse(type %in% c("review") & is.na(type_hierarchy),"review not confirmed",type),
                          type=ifelse(type %in% c("modification") & is.na(type_hierarchy),"modification not confirmed",type)) 

[..]
```

8. **Merge with lending arrangement data**

``` R
IMFprograms=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/Lending arrangements/IMF_programs_clean.RData")
```

9. **Only keep verified requests and reviews**

``` R
dt_merged=dt_merged %>% filter(!type %in% c("modification not confirmed",
                                  "request not confirmed",
                                  "review not confirmed"))
```

10. **Clean review numbers **

``` R
mydata=mydata %>% group_by(iso3c,startdate) %>% 
  #find first and last documents of each program
  mutate(first_doc=ifelse(is.na(first(Review_number)), first(type_doc),first(Review_number)), last_review_N=last(Review_number)) %>%
  ungroup() %>%
  # regroup review numbers into 3 periods: first review, midterm review and last review
  mutate(Review_number_new=ifelse(Review_number=="review_midterm" & duration_IMFprogram==1,"review",NA),
                         Review_number_new=ifelse(Review_number=="review_1" & duration_IMFprogram==1,"review",Review_number_new),
                         Review_number_new=ifelse(Review_number==last_review_N,"3 Last Review",Review_number_new),
                         Review_number_new=ifelse(type==first_doc | type=="Request","1 Request",Review_number_new),#,
                         Review_number_new=ifelse(is.na(Review_number_new) & !is.na(type),"2 midterm Review",Review_number_new),
         Review_number_new=ifelse(is.na(myID),"0 No crisis",Review_number_new))


[..]

```

11.  **Averages on the full duration of the programs **

``` R
myvars=str_remove(names(mydata)[str_detect(names(mydata),"_norm")],"_norm")
mydata=mydata %>% group_by(iso3c,startdate) %>% 
  mutate_at(vars(myvars),funs(program_mean=mean(.,na.rm=T)))
```

12.  **Export clean data**

``` R
rio::export(mydata,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData")
```


### 4. Analysis

---------

 **Description:**
 
 The script takes as input the term frequency matrix and the dataset created by 3. Clean_database.r   to perform exploratory analysis and grafical representations by sourcing differenct files
 
- 4.1. Analysis_tf_idf_decades.r:   Creates radarchart of tf-idf for each decade for shocks, structure and reforms 
 
- 4.2. Analysis_timing_shocks.r     Plots the occurence of shocks from request to midterm review and final review to observe how shocks evolve during the whole period of the debt distress
 
- 4.3; Analysis_ex_timeseries.r  Profide an example of time series for different shocks for a selected country 
 
- 4.4. Analysis_exogeneity_shocks.r Look at degree of exogeneity of shocks by looking at the correlationg between the shocks
 
- 4.5. Analysis_clusters_shocks.r  Create a cluster analysis with kmean methods
 
- 4.6. Analysis_timing_tf.r   Observe term frequency barplot segmented by review to observe the evolution of priority during the  duration of the distress

1. **Import data**

``` R
LoI_tf_idf=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")
```

2. **Complete information combining tf, url metadata and quantititve measures**  

``` R
mydata=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData")
```

3. **evolution of tf-idf across decades **

``` R
[...] 
```

4.. **Timing and sequence of the crisis **

``` R
[...] 
```

5.  **Plot time series of evolution **

``` R
ctry="URY"

[...] 
```

6.  **Exogeneity of the crisis **

``` R
[...] 
```

7. **clusters shocks **

``` R
[...] 
```

8.  **Average term frequency**

``` R
[...] 
```

9.  **save output**

``` R
[...] 
```


























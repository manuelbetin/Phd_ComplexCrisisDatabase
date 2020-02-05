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

ind_IMFprograms=function(dt){
  
  if(!any(c("title","year") %in% names(dt))){
    print("please provide a valide database containing at least the columns title and year")
   dt
  }else{
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"request"),"request",NA),
                   type_doc_programs=ifelse(str_detect(title,'arrangement under the flexible credit line'),"request",type_doc_programs),
                   #type_doc_programs=ifelse(str_detect(keywords,'requests'),"request",type_doc_programs),
                   #type_doc_programs=ifelse(str_detect(keywords,'Letters of Intent'),"request",type_doc_programs),
                   #type_doc_programs=ifelse(str_detect(keywords,'arrangement texts'),"request",type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,'letter on economic policy'),"request",type_doc_programs),
                   type_doc_programs=ifelse( str_detect(title,'stand-by arrangement') & !str_detect(title,'review'),"request",type_doc_programs),
                   type_doc_programs=ifelse( str_detect(title,'extended arrangement') & (!str_detect(title,'review') | !str_detect(title,'request for modification') | !str_detect(title,'waiver') ),"request",type_doc_programs),
                   type_doc_programs=ifelse( str_detect(title,'extended fund facility') & (!str_detect(title,'review') | !str_detect(title,'request for modification')),"request",type_doc_programs),
                   type_doc_programs=ifelse( str_detect(title,'enhanced structural adjustment') & (!str_detect(title,'review') | !str_detect(title,'request for modification')),"request",type_doc_programs),
                   type_doc_programs=ifelse( str_detect(title,'poverty reduction and growth') & (!str_detect(title,'review') | !str_detect(title,'request for modification')),"request",type_doc_programs),
                   type_doc_programs=ifelse( str_detect(title,'structural adjustment facility') & (!str_detect(title,'review') | !str_detect(title,'request for modification')),"request",type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,'request for increase in quotas'),NA,type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,'request for enhanced article iv'),NA,type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,'modification'),"modification",type_doc_programs))
  
  # find the number of the review ---------
  dt=dt %>% mutate(Review_number=ifelse(str_detect(title,"review") & str_detect(title,"first"),"review_1",
                                        ifelse(str_detect(title,"review") & str_detect(title,"second"),"review_2",
                                               ifelse(str_detect(title,"review") & str_detect(title,"third"),"review_3",
                                                      ifelse(str_detect(title,"review") & str_detect(title,"fourth"),"review_4",
                                                             ifelse(str_detect(title,"review") & str_detect(title,"fifth"),"review_5",
                                                                    ifelse(str_detect(title,"review") & str_detect(title,"sixth"),"review_6",
                                                                           ifelse(str_detect(title,"review") & str_detect(title,"seventh"),"review_8",
                                                                                  ifelse(str_detect(title,"review") & str_detect(title,"eight"),"review_9",
                                                                                         ifelse(str_detect(title,"review") & str_detect(title,"ninth"),"review_10",
                                                                                                ifelse(str_detect(title,"review") & str_detect(title,"tenth"),"review_11",NA)))))))))))
  

  dt=dt %>% mutate(Review_number=ifelse(str_detect(title,"review") & str_detect(title,"midterm"),"review_midterm",Review_number),
                   Review_number=ifelse(str_detect(title,"review") & str_detect(title,"review") & is.na(Review_number),"review",Review_number),
                   type_doc_programs=ifelse((!is.na(Review_number) & is.na(type_doc_programs)) | (str_detect(title,"review")),"review",type_doc_programs),
                   type_doc_programs=ifelse(!is.na(Review_number) & str_detect(title,"request") & !str_detect(title,"waiver"),"request and review",type_doc_programs))
  
  
  # find use of fund ressource -------
  
  dt=dt %>% mutate(type_doc_programs=ifelse(is.na(type_doc_programs) & str_detect(title,"use of fund"),"Use fund",type_doc_programs))
  
  # find cancellations -------
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"cancellation"),"cancelation",type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,"cancellation") & (str_detect(title,"request") | str_detect(title,"flexible credit line") ),"cancelation and request",type_doc_programs))
  # find extensions -------
  
  dt=dt %>% mutate(type_doc_programs=ifelse(! is.na(type_doc_programs) & (str_detect(title,"prolongation") | str_detect(title,"extension") | str_detect(title,"lengthening")| str_detect(title,"augmentation")),"extension",type_doc_programs))
  
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"request for postponement"),"request for postponement",type_doc_programs))
  
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"request for technical assistance"),"request for technical assistance",type_doc_programs))
  # find compensatory -------
  
  dt=dt %>% mutate(type_doc_programs=ifelse(is.na(type_doc_programs) & str_detect(title,"performance criteria"),"performance criteria",type_doc_programs))
  dt=dt %>% mutate(performance_criteria=ifelse(str_detect(title,"performance criteria") | str_detect(title,"performance criterion"),"performance criteria",NA))
  dt=dt %>% mutate(waiver=ifelse(str_detect(title,"waiver"),"waiver",NA))
  dt=dt %>% mutate(modification=ifelse(str_detect(title,"request for modification"),"modification",NA))
  
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"waiver") & str_detect(title,"performance criteria") & is.na(type_doc_programs),"performance criteria and waiver",type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,"waiver") & is.na(type_doc_programs),"waiver",type_doc_programs))
  
  dt=dt %>% mutate(type_doc_programs=ifelse(!is.na(type_doc_programs) & (str_detect(title,"purchase transaction")),"purchase transac",type_doc_programs))
  
  #print("New columns have been created \n
  #      type_doc_programs,Review_number")
  dt
  }
  
}

find_increase_quotas=function(dt){
  dt=dt %>% mutate(increase_quotas=ifelse(str_detect(title,"increase in quotas"),1,NA))
  dt
}

find_membership=function(dt){
  dt=dt %>% mutate(membership=ifelse(str_detect(title,"application for membership"),1,NA))
  dt
}

find_name_from_title=function(dt){
  
  if(!any(c("title","year") %in% names(dt))){
    print("please provide a valide database containing at least the columns title and year")
    dt
  }else{
  dt=dt %>% mutate(title2=str_replace(title,":","-")) %>% separate(title2,into="country",sep="-") %>% dplyr::select(iso3,country,period,title,everything())
  
  dt= dt  %>% mutate(country=str_trim(gsub('[^ -~]', '', country),"both"))
  
  ctries=countrycode::countrycode(list_countries(),origin="iso3c",destination="country.name") %>% tolower()
  
  nonstandard_ctrynames=c(COD="zaire",SOM="somalia",YEM="yemen arab republic","yugoslavia",CIV="ivory coast",WSM="western samoa",HUN="hungarian people's republic",KOR="korea",
                          MMR="burma",VCT="st. vincent and the grenadines",GMB="the gambia",CIV="cote d'ivoire",COD="people's republic of the congo",CHN="people's republic of china",
                          EGY="arab republic of egypt",MOZ="people's republic of mozambique",TTO="trinidad and tobago",STP="sao tome and principe",LAO="lao people's democratic republic",
                          MOZ="republic of mozambique",POL="republic of poland",CZE="czech and slovak federal republic",RUS='russian federation',CZE="czech republic",SVK="slovak republic",
                          LVA='republic of latvia',KGZ="kyrgyz republic",MDA="republic of moldova",VNM="viet nam",LTU="republic of lithuania",EST="republic of estonia",KAZ="republic of kazakhstan",
                          MKD="former yugoslav republic of macedonia",COG="republic of congo",HRV="republic of croatia",ARM="republic of armenia",BLR="republic of belarus",UZB="republic of uzbekistan",
                          AZE="azerbaijan republic",GEO="republic of georgia",KAZ="republic of kazakstan",BIH="republic of bosnia and herzegovina",YEM="republic of yemen",TJK="republic of tajikistan",
                          BIH="bosnia and herzegovina",KOR="republic of korea",KNA="st. kitts and nevis",GNQ="guinea bissau",MEX="mexico <U+0097> arrangement under the flexible credit line",
                          MEX="mexico<U+0097>review under the flexible credit line arrangement",COL="colombia<U+0097>review under the flexible credit line arrangement")
  
  nonstandard_ctrynames2=as.data.frame(nonstandard_ctrynames)
  nonstandard_ctrynames2$iso3c=names(nonstandard_ctrynames)
  names(nonstandard_ctrynames2)=c("iso3_new","iso3c")
  nonstandard_ctrynames2=nonstandard_ctrynames2 %>% mutate(iso3_new=as.character(iso3_new))
  
  dt=dt %>% mutate(iso3_error=ifelse(!country %in% c(ctries,nonstandard_ctrynames),country,""),
                   iso3_new=as.character(ifelse(country %in% c(ctries,nonstandard_ctrynames),country,"")))
  
  dt=dt %>% left_join(nonstandard_ctrynames2,by=c("iso3_new"))
  
  #correct manually some cases and transform to iso3c
  dt=dt %>% mutate(iso3c=ifelse(is.na(iso3c),countrycode::countrycode(iso3_new,origin="country.name",destination="iso3c"),iso3c),
                   iso3c=ifelse(str_detect(iso3,"mexico"),"MEX",iso3c),
                   iso3c=ifelse(str_detect(iso3,"philippines"),"PHL",iso3c),
                   iso3c=ifelse(str_detect(iso3,"macedonia"),"MKD",iso3c),
                   iso3c=ifelse(str_detect(iso3,"yugoslavia"),"YUG",iso3c))
  
  
  dt=dt %>% dplyr::select(-c(iso3_new,iso3_error)) %>% rename(iso3_from_title=iso3c) %>%
    dplyr::select(iso3,country,iso3_from_title,period,year,pdf,everything())
  }
  dt
}

find_repurchase=function(dt){
  dt=dt %>% mutate(repurchase_transaction=ifelse(str_detect(title,"repurchase transaction"),1,NA))
  dt
}

find_consultations=function(dt){
  
  if(!any(c("title","year") %in% names(dt))){
    print("please provide a valide database containing at least the columns title and year")
    dt
  }else{
    
  # find non program reports  
  dt=dt %>% mutate(type_doc_consultations=ifelse(str_detect(title,"article iv consultation"),"Article IV",
                                             ifelse(str_detect(title,"article xiv consultation"),"Article XIV",
                                                   ifelse(str_detect(title,"recent economic developments"),"Eco developments",
                                                          ifelse(str_detect(title,"selected issues"),"Selected issues",
                                                                 ifelse(str_detect(title,"article viii"),"Article VIII",
                                                                        ifelse(str_detect(title,"background papers"),"Article IV",NA)))))))
  
 dt=dt %>% mutate(type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"consultations"),"consultations",type_doc_consultations))
 dt=dt %>% mutate(type_doc_consultations=ifelse(str_detect(title,"summing up"),NA,type_doc_consultations),
                  type_doc_consultations=ifelse(str_detect(title,"stand-by arrangement"),NA,type_doc_consultations)) 
 
 dt=dt %>% mutate(type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"exchange system"),"exchange system",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"exchange rate adjustment"),"exchange system",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"exchange arrangement"),"exchange system",type_doc_consultations))
  dt
  }
}

find_statements=function(dt){
  dt=dt %>% mutate(statements=ifelse(str_detect(title,"statement"),1,NA))
  dt
}

find_technical_assistance=function(dt){
  dt=dt %>% mutate(technical_assistance=ifelse(str_detect(title,"technical assistance"),1,NA))
  dt
}

find_board_governors=function(dt){
  dt=dt %>% mutate(board_governors=ifelse(str_detect(title,"board of governors"),1,NA))
  dt
}

find_overdue_financial_obligations=function(dt){
  dt=dt %>% mutate(overdue_obligations=ifelse(str_detect(title,"overdue financial obligations"),1,NA))
  dt
}

find_expost_assesments=function(dt){
  dt=dt %>% mutate(expost_assessment=ifelse(str_detect(title,"ex post assessment"),1,
                                            ifelse(str_detect(title,"post-program"),1,NA)))
  dt
}

find_exchange_system=function(dt){
  dt=dt %>% mutate(exchange_system=ifelse(str_detect(title,"exchange system"),1,
                                          ifelse(str_detect(title,"exchange rate adjustment"),1,
                                                            ifelse(str_detect(title,"exchange arrangements"),1,NA))))
  dt
}

find_program_type=function(dt){
  
  dt=dt %>% mutate(type_program=ifelse(str_detect(title,"extended fund facility"),"EFF",
                                                       ifelse(str_detect(title,"esaf arrangement"),"ESAF",
                                                              ifelse(str_detect(title,"systemic transformation facility"),"STF",
                                                                     ifelse(str_detect(title,"poverty reduction and growth facility"),"PRGF",
                                                                            ifelse(str_detect(title,"flexible credit line"),"FCL",
                                                                                   ifelse(str_detect(title,"precautionary and liquidity Line") | str_detect(title,"precautionary credit line") ,"PLL",
                                                                                          ifelse(str_detect(title,"saf arrangement"),"SAF",
                                                                                                 ifelse(str_detect(title,"stand-by arrangement") | str_detect(title,"stand-by arrangement") | str_detect(title,"stand-by arrangement"),"SBA",
                                                                                                        ifelse(str_detect(title,"extended arrangement"),"EA","Other"))))))))))
  
  
  dt=dt %>% mutate(type_program=ifelse(is.na(type_program) & str_detect(title,"extended fund facility"),"EFF",type_program),
                                   type_program=ifelse(is.na(type_program) & str_detect(title,"extended arrangement"),"EA",type_program))
  dt
  }

create_file_name=function(dt){
  dt=dt %>% mutate(file=paste0(iso3_from_title,"_",period,"_",ifelse(!is.na(type_doc_programs),type_doc_programs,
                                                                     ifelse(!is.na(type_doc_consultations),type_doc_consultations,"other_doc"))))
  dt
  }

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


























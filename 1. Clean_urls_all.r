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
              "ggplot2",
              'crayon',
              "TextMiningCrisis") #make sure you have the library 

## load common packages
load.my.packages(packages)

#-----------
clean_IMF_urls=function(file){
  #label the type of EBS: cleaned, correction or supplement
  EBS_files=url_links %>% filter(str_detect(hierarchy,"EBS"))
  EBS_files_cor=EBS_files %>% filter(str_detect(hierarchy,"Cor")) %>% mutate(type_hierarchy="Correction")
  EBS_files_sup=EBS_files %>% filter(str_detect(hierarchy,"Sup")) %>% mutate(type_hierarchy="Supplement")
  EBS_files_clean=EBS_files %>% filter(!(str_detect(hierarchy,"Sup") | str_detect(hierarchy,"Cor"))) %>% mutate(type_hierarchy="Clean")
  final_dt=rbind(EBS_files_clean,EBS_files_cor,EBS_files_sup)
  final_dt=final_dt %>% mutate(date=as.Date(date,format="%b %d %Y"))
  final_dt
}

find_keyword_list=function(files){
  #find the full set of keywords available in the metadata of the documents
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

name_links_dt="IMFSBA_Reviews_links.csv"#
url_links=rio::import(paste0("files/IMF_urls_raw/",name_links_dt))
url_links=clean_IMF_urls(url_links)

# clean url links ------

urls_clean=url_links #%>% filter(type_hierarchy %in% c("Clean")) #select links corresponding to EBS with no correction or supplements
myIMF_keywords=find_keyword_list(urls_clean) #find the list of keywords across the dataset

#urls_clean=urls_clean %>%  filter(str_detect(keywords,"ESAF arrangement requests"))
                                  
#transform title to lower case
urls_clean=urls_clean %>% mutate(title=tolower(title))

# find country name from title ####
urls_clean=urls_clean %>% mutate(title2=title) %>% separate(title2,into="iso3",sep="-") %>% dplyr::select(iso3,date,everything())
urls_clean=urls_clean %>% mutate(iso3=str_trim(iso3,"both"))
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

urls_clean=urls_clean %>% mutate(iso3_error=ifelse(!iso3 %in% c(ctries,nonstandard_ctrynames),iso3,""),
                                 iso3_new=as.character(ifelse(iso3 %in% c(ctries,nonstandard_ctrynames),iso3,"")))

urls_clean=urls_clean %>% left_join(nonstandard_ctrynames2,by=c("iso3_new"))

#correct manually some cases and transform to iso3c
urls_clean=urls_clean %>% mutate(iso3c=ifelse(is.na(iso3c),countrycode::countrycode(iso3_new,origin="country.name",destination="iso3c"),iso3c),
                                 iso3c=ifelse(str_detect(iso3,"mexico"),"MEX",iso3c),
                                 iso3c=ifelse(str_detect(iso3,"philippines"),"PHL",iso3c),
                                 iso3c=ifelse(str_detect(iso3,"macedonia"),"MKD",iso3c),
                                 iso3c=ifelse(str_detect(iso3,"yugoslavia"),"YUG",iso3c))
urls_clean=urls_clean %>% filter(!is.na(iso3c)) %>%
  dplyr::select(iso3c,-iso3_new,-iso3,date,everything()) %>% dplyr::select(-c(iso3_error,iso3_new))


# find the requests ---------

urls_clean=urls_clean %>% mutate(type_doc=ifelse(str_detect(title,"request")& !str_detect(title,"waiver"),"request",NA),
                                 type_doc=ifelse(str_detect(title,'arrangement under the flexible credit line'),"request",type_doc),
                                 type_doc=ifelse(str_detect(keywords,'requests'),"request",type_doc),
                                 type_doc=ifelse(str_detect(keywords,'arrangement texts'),"request",type_doc),
                                 type_doc=ifelse(str_detect(title,'letter on economic policy'),"request",type_doc))

# find the number of the review ---------
urls_clean=urls_clean %>% mutate(Review_number=ifelse(str_detect(title,"review") & str_detect(title,"first"),"review_1",
                                                      ifelse(str_detect(title,"review") & str_detect(title,"second"),"review_2",
                                                             ifelse(str_detect(title,"review") & str_detect(title,"third"),"review_3",
                                                                    ifelse(str_detect(title,"review") & str_detect(title,"fourth"),"review_4",
                                                                           ifelse(str_detect(title,"review") & str_detect(title,"fifth"),"review_5",
                                                                                  ifelse(str_detect(title,"review") & str_detect(title,"sixth"),"review_6",
                                                                                         ifelse(str_detect(title,"review") & str_detect(title,"seventh"),"review_8",
                                                                                                ifelse(str_detect(title,"review") & str_detect(title,"eight"),"review_9",
                                                                                                       ifelse(str_detect(title,"review") & str_detect(title,"ninth"),"review_10",
                                                                                                              ifelse(str_detect(title,"review") & str_detect(title,"tenth"),"review_11",NA)))))))))))

urls_clean=urls_clean %>% mutate(Review_number=ifelse(str_detect(title,"review") & str_detect(title,"midterm"),"review_midterm",Review_number),
                                 Review_number=ifelse(str_detect(title,"review") & str_detect(title,"review") & is.na(Review_number),"review",Review_number),
                                 type_doc=ifelse((!is.na(Review_number) & is.na(type_doc)) | str_detect(keywords,"reviews"),"review",type_doc),
                                 type_doc=ifelse(!is.na(Review_number) & str_detect(title,"request") & !str_detect(title,"waiver"),"request and review",type_doc))

# find use of fund ressource -------

urls_clean=urls_clean %>% mutate(type_doc=ifelse(str_detect(title,"use of fund resources"),"Use fund",type_doc))

# find cancellations -------
urls_clean=urls_clean %>% mutate(type_doc=ifelse(str_detect(title,"cancellation"),"cancelation",type_doc),
                                 type_doc=ifelse(str_detect(title,"cancellation") & str_detect(title,"request"),"cancelation and request",type_doc))

# find extensions -------

urls_clean=urls_clean %>% mutate(type_doc=ifelse(str_detect(title,"prolongation") | str_detect(title,"extension") | str_detect(title,"lengthening"),"extension",type_doc))

# find compensatory -------

urls_clean=urls_clean %>% mutate(type_doc=ifelse(str_detect(title,"compensatory"),"compensatory",type_doc))

urls_clean=urls_clean %>% mutate(type_doc=ifelse(is.na(type_doc) & str_detect(title,"performance criteria"),"performance criteria",type_doc))
urls_clean=urls_clean %>% mutate(performance_criteria=ifelse(str_detect(title,"performance criteria") | str_detect(title,"performance criterion"),"performance criteria",NA))
urls_clean=urls_clean %>% mutate(waiver=ifelse(str_detect(title,"waiver"),"waiver",NA))
urls_clean=urls_clean %>% mutate(type_doc=ifelse(str_detect(title,"waiver") & str_detect(title,"performance criteria") & is.na(type_doc),"performance criteria and waiver",type_doc),
                                 type_doc=ifelse(str_detect(title,"waiver") & is.na(type_doc),"waiver",type_doc))

a=urls_clean %>% filter(!str_detect(hierarchy,"Sup") & is.na(type_doc))
a=urls_clean %>% group_by(type_doc) %>% summarize(n=n())


# type of program --------

urls_clean=urls_clean %>% mutate(type_program=ifelse(str_detect(keywords,"Extended arrangement"),"EA",
                                                     ifelse(str_detect(keywords,"Stand-by arrangement") | str_detect(keywords,"Stand-By Arrangements"),"SBA",
                                                            ifelse(str_detect(keywords,"Purchases under compensatory financing"),"CF",
                                                                   ifelse(str_detect(keywords,"Buffer Stock Financing Facility"),"BSFF",
                                                                          ifelse(str_detect(keywords,"ESAF arrangement"),"ESAF",
                                                                                 ifelse(str_detect(keywords,"Emergency purchase transactions"),"EPT",
                                                                                        ifelse(str_detect(keywords,"Structural adjustment arrangement requests"),"SAA",
                                                                                               ifelse(str_detect(keywords,"Purchases under Compensatory and Contingency Financing Facility") | str_detect(keywords,"Compensatory and Contingency Financing Facility"),"CCFF",
                                                                                                      ifelse(str_detect(keywords,"Systemic Transformation Facility"),"STF",
                                                                                                             ifelse(str_detect(keywords,"Emergency assistance") | str_detect(keywords,"Post-conflict emergency assistance"),"EAssist",
                                                                                                                    ifelse(str_detect(keywords,"Poverty Reduction and Growth Facility"),"PRGF",
                                                                                                                           ifelse(str_detect(keywords,"Flexible credit line"),"FCL",
                                                                                                                                  ifelse(str_detect(keywords,"Precautionary and Liquidity Line") | str_detect(keywords,"Precautionary Credit Line") ,"PLL",
                                                                                                                                         ifelse(str_detect(keywords,"SAF arrangement"),"SAF",
                                                                                                                                                ifelse(str_detect(keywords,"first credit tranche"),"FCT",
                                                                                                                                                       ifelse(str_detect(keywords,"Extended Credit Facility"),"ECF",NA)))))))))))))))))



urls_clean=urls_clean %>% mutate(type_program=ifelse(is.na(type_program) & str_detect(title,"extended fund facility"),"EFF",type_program),
                                 type_program=ifelse(is.na(type_program) & str_detect(title,"extended arrangement"),"EA",type_program))

#remove programs not corresponding to any of the above programss

urls_clean=urls_clean %>% filter(!is.na(type_program))

a=urls_clean %>% group_by(type_doc) %>% summarize(n=n()) %>% filter(type_doc=="review")

#remove corrections

urls_clean= urls_clean %>% filter(!(str_detect(hierarchy,"Cor") | str_detect(hierarchy,"Sup") | is.na(type_doc)))

urls_clean=urls_clean %>% dplyr::select(iso3c,date,hierarchy,type_program,type_doc,Review_number,performance_criteria,waiver,everything()) %>% arrange(iso3c,date)

#remove duplicates

urls_clean=urls_clean %>% dplyr::select(-c(V1,cfs,subject,title_1))
urls_clean=urls_clean %>% distinct()

# create variables for type of document -----

type_doc_list=function(){
type_doc=c("Nonapproval recommended by staff",
           'Fund approval extension proposed',
           "Fund approval",
           
           "Letters of Intent",
           'Debt sustainability analysis',
           'Press Releases',
           'ARTICLE VIII',
           "Article IV consultations",
           'Article IV',
           "Staff Reports",
           "Staff appraisals",
           'Post-program monitoring',
           
           'Fund financial position reviews',
           'Fund liquidity position',
           
           "Performance criteria waivers",
           "Performance criteria",
           'Performance clauses',
           "Performance criteria modifications",
           
           "Stand-by arrangement requests",
           "Stand-by arrangement reviews",
           "Stand-by arrangement extension",
           "Stand-by arrangement cancellations",
           "Stand-by arrangement text amendments",
           
           "Extended arrangements",
           'Extended arrangement requests',
           'Extended arrangement cancellations',
           'Extended arrangement text amendments',
           'Extended arrangement reviews',
           "Extended arrangement extension",
           "Extended Fund Facility",
           
           "Extended Credit Facility",
           "Rapid Credit Facility",
           "Emergency assistance",
           
           "ESAF arrangements",
           'ESAF arrangement negotiations missions',
           'ESAF arrangement requests',
           'ESAF arrangement extension',
           
           'Enhanced Structural Adjustment Facility',
           'Structural adjustment arrangement requests',
           "ESAF arrangement reviews",
           'Structural Adjustment Facility',
           
           "Technical assistance",
           'Exogenous Shocks Facility',
           'Standby Credit Facility',
           'Flexible credit line',
           'External contingency mechanisms',
           
           'Commodity stabilization funds',
           'Post-conflict emergency assistance',
           'Compensatory financing of export fluctuations',
           'Compensatory and Contingency Financing Facility',
           'Systemic Transformation Facility',
           'Poverty Reduction and Growth Facility'
            
           )
return(type_doc)
}

type_doc=type_doc_list()

for(i in 1:length(type_doc)){
  namevar=str_replace_all(type_doc[i]," ","_")
  urls_clean=urls_clean %>% mutate(!!namevar:=ifelse(str_detect(keywords,type_doc[i]),1,0))
}

# Descriptives ----

name_links_dt='IMFECF_EA1950_Reviews_links.csv' #"IMFECF_SBA1950_Requests_links.csv"#"IMFECF_Requests_links.csv" #"IMFSBA_Reviews_links.csv"#"all_links.csv"
url_links=rio::import(paste0("files/IMF_urls_raw/",name_links_dt))
names(url_links)[1]="title2"
url_links=url_links %>% mutate(date=as.Date(date,format="%b %d %Y")) %>% mutate(year=lubridate::year(date))

row_links_by_year=url_links %>% group_by(year) %>% summarize(n=n())
ggplot(row_links_by_year)+
  geom_bar(stat="identity",aes(x=year,y=n))+
  theme_bw()

getwd()
name_links_dt="IMFSBA_Reviews_links_clean.Rdata"#"IMFECF_Requests_links.csv" #"IMFSBA_Reviews_links.csv"#"all_links.csv"
url_links=rio::import(paste0("files/IMF_urls_clean/",name_links_dt))
url_links=url_links %>% mutate(date=as.Date(date,format="%b %d %Y")) %>% mutate(year=lubridate::year(date))
#urls_clean=urls_clean  %>% mutate(date=as.Date(date,format="%b %d %Y")) %>% mutate(year=lubridate::year(date))
head(url_links$type_hierarchy %>% unique())
programs_by_year=IMF_programs  %>% mutate(year=lubridate::year(Period)) %>% group_by(year) %>% summarize(n=n())
links_by_year=url_links %>% group_by(year) %>% filter(type_hierarchy %in% c("Clean","Supplements")) %>% summarize(n=n())
ggplotly(ggplot()+
           geom_bar(data=IMF_links_old_by_year,stat="identity",aes(x=year,y=n,fill="old_urls"),color="black",alpha=0.5)+
           geom_bar(data=links_by_year,stat="identity",aes(x=year,y=n,fill="urls"),color="black")+
           geom_point(data=programs_by_year,stat="identity",aes(x=year,y=n),color="black",size=2)+
           labs(title="Requests")+
           lims(y=c(0,45))+
           theme_bw())


names(url_links)

years_availables=lubridate::year((urls_clean %>% arrange(date))$date) %>% unique()

#Export ------

rio::export(urls_clean,"files/All_links_clean.csv")




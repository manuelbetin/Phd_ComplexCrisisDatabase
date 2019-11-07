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

#-----------
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

name_links_dt="IMFECF_Requests_links.csv"
url_links=rio::import(paste0("files/",name_links_dt))
url_links=clean_IMF_urls(url_links)

# clean url links ------

urls_clean=url_links %>% filter(type_hierarchy %in% c("Clean")) #select links corresponding to EBS with correction or supplements
myIMF_keywords=find_keyword_list(urls_clean) #find the list of keywords across the dataset

#filter "standby arrangements reviews" and remove unrelevant documents
urls_clean=urls_clean %>%  filter(str_detect(keywords,"Extended arrangement requests") &
                                    str_detect(keywords,"Extended Credit Facility"))
                                  
#transform title to lower case
urls_clean=urls_clean %>% mutate(title=tolower(title))
# keep only files with word review in the title
urls_clean=urls_clean %>%  filter(str_detect(title,"request"))
#find country name from title
urls_clean=urls_clean %>% mutate(title2=title) %>% separate(title2,into="iso3",sep="-") %>% dplyr::select(iso3,date,everything())
#find the number of the review

urls_clean=urls_clean %>% arrange(iso3,date)

urls_clean=urls_clean %>% dplyr::select(iso3,date,hierarchy,everything())

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
           
           'ESAF arrangement negotiations missions',
           'ESAF arrangement requests',
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

# create lexicon of IMF keywords -----

nonrelevant_keywords_list=function(){
  nonrelevant_keywords=c(
    "Lapse of time consideration",
    "Western Hemisphere Department",
    'Waivers under Article V Section 3(b)(iii)',
    "Asia and Pacific Department",
    "Travel",
    'Fund-supported adjustment programs',
    "Caribbean Development Bank",
    "Catastrophe Containment and Relief Trust",
    'Phasing of purchases',
    "Asian Development Bank",
    'Democratic Republic of',
    'S?o Tom? and Pr?ncipe',
    "SAF negotiations missions",
    "Data quality assessment framework",
    "Union of the",
    "European Bank for Reconstruction and Development",
    '""Article IV consultation reports',
    "and Review Department",
    "Fiscal Affairs Department",
    "Islamic State of",
    "Statistics",
    "Lapse of time approval",
    "African Development Bank",
    '""Extended Credit Facility',
    "Policy and Review Department Strategy",
    "Purchases under Systemic Transformation Facility",
    'Purchases under Systemic Transformation Facility',
    'Purchases under Compensatory and Contingency Financing Facility',
    "Asian Department",
    "Use of Fund resources",
    'ESAF arrangement review missions',
    'Exchange rate policy reviews',
    'Exchange allowances for invisible payments',
    'Exchange and Trade Relations Department',
    'Jamaica dollar',
    'Policy Support Instrument',
    'Structural adjustment arrangement reviews',
    "Stand-by Arrangement Negotiations Missions",
    'Stand-by Arrangement Review Missions',
    'Extended arrangement negotiations missions',
    'Moroccan dirham',
    'Compensatory financing of fluctuations in cost of cereal imports',
    'Parallel missions',
    'Executive Board decisions',
    'Tanzania shilling',
    'Liberian dollar',
    'Policy and Review Department Strategy',
    'Algerian dinar',
    'South African rand',
    'Malagasy franc',
    'Trinidad and Tobago',
    'South African rand',
    'Thai baht',
    'Ukrainian hryvnia',
    'Western Samoa tala',
    'rticle IV consultations',
    'Zimbabwe dollar',
    'Special Data Dissemination Standard',
    'Policy and Review Department Strategy',
    'Senegal CFA franc',
    'Solomon Islands dollar',
    'European Department',
    'Executive Board Document (Policy)',
    'Emergency financing mechanism',
    'STF purchase missions',
    'The',
    'Bangladesh taka',
    'Policy and Review Department Strategy',
    'Article IV consultation missions',
    'Legal Department',
    'Central Asia Department',
    'Secretarys Department',
    'Haitian gourde',
    'Honduran lempira',
    'Iraqi dinar',
    'Mexican peso',
    'Papua New Guinea kina',
    'Portuguese escudo',
    'Bosnia and Herzegovina',
    'Rep. of',
    'Republica Bolivariana de',
    'Resident Representatives',
    'Review clauses',
    'Retention quotas',
    'Relationships with Fund',
    'Policy Development and Review Department',
    'Poverty Reduction Strategy Papers',
    'SECTION 3',
    'Executive Board Specials (EBS)""',
    '""Stand-by arrangement reviews',
    'Islamic Republic of',
    'Czech Republic',
    'Czech and Slovak Federal Republic',
    'SECTION 5',
    'St. Kitts and Nevis',
    'Ghanaian cedi',
    'Malawi kwacha',
    'Zambian kwacha',
    'Costa Rican colon',
    'Russian Federation',
    'Korea',
    'Kosovo',
    'FYR',
    'Guinean franc',
    'Hungarian forint',
    'Equatorial Guinean ekwele',
    'Kenya shilling',
    'Kingdom of',
    'Korean won',
    'Kyrgyz Republic',
    'Mali franc',
    'Mauritanian ouguiya',
    'Mauritian rupee',
    'Romanian leu',
    'Somali shilling',
    'The Gambia',
    'Uganda shilling',
    'United Republic of',
    'Yugoslav dinar',
    'Policy and Review Department Strategy',
    '""Secretarys Department""',
    '""Treasurers Department""',
    'African Department',
    'DP',
    'West African Monetary Union',
    'Western Hemisphere Dept.',
    'Annual reviews',
    'Article IV consultation reports',
    'Article XIV consultations',
    'Barbados dollar',
    'Belize dollar',
    'Chilean peso',
    'Dominican peso',
    'Ecuadoran sucre',
    'Czechoslovakia',
    'Antigua and Barbuda',
    'former Yugoslav Republic of',
    'C<U+00F4>te dIvoire',
    'Article IV consultation reports',
    'Article IV consultation reports',
    'Executive Board Specials (EBS)""',
    '""Stand-by arrangement reviews',
    'Stand-By Arrangements',
    'Executive Board Document (Country)',
    '(Disabled) European I Departme',
    'Executive Board Specials (EBS)',
    'Fund approval proposed',
    'Access to Fund general resources',
    "Middle East and Central Asia Dept",
    "Middle Eastern Department",
    'Southeast Asia and Pacific Department',
    "Asian Department",
    "Asia and Pacific Department 1",
    'Middle East and Central Asia Department',
    '(Disabled) European I Departme',
    'Upper credit tranche arrangements',
    'Obligations of members',
    'Policy and Review Department Strategy',
    'Enlarged access policy',
    "(Disabled) European II Departm",
    "Congo",
    'Cote dIvoire',
    'Dem. Rep. of',
    'Dem. Rep. of',
    'Federal Republic of (Serbia/Montenegro)',
    'Somalia',
    'Overdue repurchase obligations',
    'Exchange allowances for travel',
    "The Republic of",
    '(Disabled) European I Departme',
    'Yugoslavia',
    'Democratic Republic of the',
    'African Department',
    'Republic of',
    'Mr. Mozhin Office of Executive Directors',
    "Mr. Mozhin Office of Executive Directors")
  
}
nonrelevant_keywords=nonrelevant_keywords_list()
countries=countrycode::countrycode(SetUpProject::list_countries(sample="All"),origin="iso3c",destination="country.name")

IMF_details_program=myIMF_keywords$summary_keywords %>% filter(!keywords %in% c(type_doc,nonrelevant_keywords,countries) & n>1)
rio::export(IMF_details_program,"files/IMF_keywords/IMFECF_keywords.RData")

#create variables based on IMF keywords

for(i in 1:length(IMF_details_program$keywords)){
  namevar=str_replace_all(IMF_details_program$keywords[i]," ","_")
  if(namevar!=""){
  urls_clean=urls_clean %>% mutate(!!namevar:=ifelse(str_detect(keywords,IMF_details_program$keywords[i]),1,0))
  }
}

rio::export(urls_clean,"files/IMFECF_Requests_links_clean.Rdata")


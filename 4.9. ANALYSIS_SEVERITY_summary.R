


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)

library(forcats)


shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")


# summary table with all dimensions


severity_summary_table=function(mydata,ctry="FRA",shocks,lowerbound=0){
  #' @title summary table of severity measures
  #' @describeIn Table summarizing probability, intensity
  #' duration and complexity
  #' @param mydata the tf.idf database 
  #' @param ctry the country to which display the figure
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @return ggplot object
  #' @author Manuel Betin
  #' @export
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  probability=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    filter(ISO3_Code%in%ctry& year>=1945) %>%
    ungroup()%>%
    mutate_at(vars(shocks), get_prob) %>% 
    group_by(ISO3_Code) %>%
    summarize_at(vars(shocks),mean,na.rm=T) %>%
    gather(key="shocks",value = "value",-"ISO3_Code") %>% dplyr::rename(probability=value)
  
  cond_mean=function(x){
    mean(ifelse(x<lowerbound,NA,x),na.rm=T)
  }
  
  intensity=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    filter(ISO3_Code%in%ctry& year>=1945) %>% ungroup() %>%
    group_by(ISO3_Code) %>%
    summarise_at(vars(shocks),cond_mean) %>%
    gather(key="shocks",value=value,-"ISO3_Code")%>% dplyr::rename(intensity=value) %>%
    group_by(ISO3_Code) %>%
    mutate(intensity=as.numeric(intensity),
           priority=intensity/sum(intensity,na.rm=T)) %>% dplyr::select(-intensity)
  

  myduration=get_duration(mydata %>% filter(ISO3_Code%in%ctry),shocks)
  myduration=myduration %>%
    group_by(ISO3_Code,shocks) %>%
    summarize(persistence=mean(duration,na.rm=T) %>% round(.,2)) %>%
    arrange(-persistence) %>% filter(!is.na(shocks)) %>% arrange(shocks)
 
  mycomplexity=lapply(ctry,function(x){
    mycomplexity=mydata  %>%
      filter(ISO3_Code%in%x& year>=1945) %>%
      ungroup() %>%
      dplyr::select(shocks) %>% na.omit() %>%
      cor() %>% data.frame()
    mycomplexity[is.na(mycomplexity)]=0
    mycomplexity%>% summarise_at(vars(shocks),mean,na.rm=T) %>%
      gather(key="shocks",value="complexity") %>%
      mutate(ISO3_Code=x)
  })
  mycomplexity=do.call(rbind,mycomplexity)
  
  summary_crisis=probability %>% left_join(intensity,by=c("ISO3_Code","shocks"))
  summary_crisis=summary_crisis %>% left_join(myduration,by=c("ISO3_Code","shocks"))
  summary_crisis=summary_crisis %>% left_join(mycomplexity,by=c("ISO3_Code","shocks"))
  
  summary_crisis[is.na(summary_crisis)]=0
  
  summary_crisis=summary_crisis %>%
    group_by(shocks) %>%
    mutate(probability_rank=rank(probability)/max(rank(probability)),
        priority_rank=rank(priority)/max(rank(priority)),
        persistence_rank=rank(persistence)/max(rank(persistence)),
        complexity_rank=rank(complexity)/max(rank(complexity)),
        area=probability_rank*priority_rank*persistence_rank*complexity_rank) %>% ungroup() %>%
    mutate(shocks=ifelse(shocks=="Balance_payment_crisis","B.o.P.",shocks),
           shocks=ifelse(shocks=="World_outcomes","World",shocks),
           shocks=ifelse(shocks=="Sovereign_default","Sovereign",shocks),
           shocks=ifelse(shocks=="Natural_disaster","Nat. disaster",shocks),
           shocks=ifelse(shocks=="Currency_crisis_severe","Currency",shocks),
           shocks=ifelse(shocks=="Soft_recession","Eco. slowdown",shocks),
           shocks=ifelse(shocks=="Severe_recession","Eco. recession",shocks),
           shocks=gsub("_","",shocks),
           shocks=gsub("crisis","",shocks)) %>% 
    arrange(-area) %>% arrange(ISO3_Code)
  
  return(summary_crisis)
}

ctries=mydata$ISO3_Code %>% unique()
severity=severity_summary_table(mydata,ctries,shocks)


severity_all=severity %>% ungroup() %>% group_by(shocks) %>% 
  summarise_at(vars(probability:area),mean) %>% ungroup()%>%
  mutate(probability=round(probability,2),
         priority=round(priority,2),
         persistence=round(persistence,2),
         complexity=round(complexity,2),
         probability_rank=round(probability_rank,2),
         priority_rank=round(priority_rank,2),
         persistence_rank=round(persistence_rank,2),
         complexity_rank=round(complexity_rank,2),
         area=round(area,2)) %>% arrange(shocks) %>% rename(crisis=shocks) %>%
  dplyr::select(-c(probability_rank,priority_rank,persistence_rank,complexity_rank,area))

stargazer::stargazer(title="Severity:"
                     , severity_all
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary.tex")
)

footnote=c("The table display the average cross countries for all severity dimensions for high income 
countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
           allocated to each crisis, persistence the average duration and complexity the average correlation.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_footnote.tex"))


#high income

ctries=ctry_groups %>% filter(Income_group==" High income")
ctries=ctries$iso3c
severity_HighIncome=severity %>% filter(ISO3_Code %in% ctries) %>%
   ungroup() %>% group_by(shocks) %>% 
  summarise_at(vars(probability:area),mean) %>% ungroup()%>%
  mutate(probability=round(probability,2),
         priority=round(priority,2),
         persistence=round(persistence,2),
         complexity=round(complexity,2),
         probability_rank=round(probability_rank,2),
         priority_rank=round(priority_rank,2),
         persistence_rank=round(persistence_rank,2),
         complexity_rank=round(complexity_rank,2),
         area=round(area,2)) %>% arrange(-area)%>% arrange(-area) %>%
  dplyr::select(crisis=shocks,probability,rank=probability_rank,
                priority,rank=priority_rank,
                persistence,rank=persistence_rank,
                complexity,rank=complexity_rank,
                area)

stargazer::stargazer(title="Severity: High Income"
                     , severity_HighIncome
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_HighIncome.tex")
)

footnote=c("The table display the average cross countries for all severity dimensions for high income 
countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
allocated to each crisis, persistence the average duration and complexity the average correlation.
The columns rank show for each dimension the average percentile of countries in the high income groups compare to all countries.
The column Area is an aggregate measure constructed by multipling the ranking of all first dimension.
Housing crisis is the highest ranking crisis among high income countries according to the aggregate index. The highest average probability
is economic slowdowns, with a 54 percent probability on average and ranking on average in the 70th percentile. 
The highest average priority is sovereign crisis with an average of 18 percent although high income countries ranks only in the
28th percentile in order of priorities. The more persistent crisis is contagion crisis with an average of 3.1 years and an average ranking
of 50 percent. The more complex crisis are Financial crisis with an average contemporaneous correlation with all other
crisis of 28 percent that locate it in the 64th percentile.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_HighIncome_footnote.tex"))


#middle income

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
ctries=ctries$iso3c
severity_MiddleIncome=severity %>% filter(ISO3_Code %in% ctries) %>%
  ungroup() %>% group_by(shocks) %>% 
  summarise_at(vars(probability:area),mean) %>% ungroup()%>%
  mutate(probability=round(probability,2),
         priority=round(priority,2),
         persistence=round(persistence,2),
         complexity=round(complexity,2),
         probability_rank=round(probability_rank,2),
         priority_rank=round(priority_rank,2),
         persistence_rank=round(persistence_rank,2),
         complexity_rank=round(complexity_rank,2),
         area=round(area,2)) %>% arrange(-area) %>%
  arrange(-area) %>%
  dplyr::select(crisis=shocks,probability,rank=probability_rank,
                priority,rank=priority_rank,
                persistence,rank=persistence_rank,
                complexity,rank=complexity_rank,
                area)

stargazer::stargazer(title="Severity: Middle Income"
                     , severity_MiddleIncome
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_MiddleIncome.tex")
)

footnote=c("The table display the average cross countries for all severity dimensions for high income 
countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
allocated to each crisis, persistence the average duration and complexity the average correlation.
The columns rank show for each dimension the average percentile of countries in the high income groups compare to all countries.
The column Area is an aggregate measure constructed by multipling the ranking of all first dimension.
Currency crisis is the highest ranking crisis among middle income countries according to the aggregate index. The highest average probability
is sovereign crisis, with a 64 percent probability on average and ranking on average in the 55th percentile. 
The highest average priority is sovereign crisis with an average of 37 percent, locating middle income countries in the
51th percentile in order of priorities. The more persistent crisis are sovereign with an average of 5.5 years and an average ranking
in the 51th percentile. The more complex crisis are contagion crisis with an average contemporaneous correlation with all other
crisis of 27 percent that locate it in the 53th percentile.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_MiddleIncome_footnote.tex"))



#Low income

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
ctries=ctries$iso3c
severity_LowIncome=severity %>% filter(ISO3_Code %in% ctries) %>%
  ungroup() %>% group_by(shocks) %>% 
  summarise_at(vars(probability:area),mean) %>% ungroup()%>%
  mutate(probability=round(probability,2),
         priority=round(priority,2),
         persistence=round(persistence,2),
         complexity=round(complexity,2),
         probability_rank=round(probability_rank,2),
         priority_rank=round(priority_rank,2),
         persistence_rank=round(persistence_rank,2),
         complexity_rank=round(complexity_rank,2),
         area=round(area,2)) %>% arrange(-area) %>%
  arrange(-area) %>%
  dplyr::select(crisis=shocks,probability,rank=probability_rank,
                priority,rank=priority_rank,
                persistence,rank=persistence_rank,
                complexity,rank=complexity_rank,
                area)

stargazer::stargazer(title="Severity: Low Income"
                     , severity_LowIncome
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_LowIncome.tex")
)

footnote=c("The table display the average cross countries for all severity dimensions for high income 
countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
allocated to each crisis, persistence the average duration and complexity the average correlation.
The columns rank show for each dimension the average percentile of countries in the high income groups compare to all countries.
The column Area is an aggregate measure constructed by multipling the ranking of all first dimension.
Epidemic crisis is the highest ranking crisis among low and lower middle income countries according to the aggregate index. The highest average probability
is sovereign crisis, with a 72 percent probability on average and ranking on average in the 65th percentile. 
The highest average priority is sovereign crisis with an average of 50 percent, locating low and lower middle income countries in the
66th percentile in order of priorities. The more persistent crisis are sovereign with an average of 6.6 years and an average ranking
in the 64th percentile. The more complex crisis are contagion crisis with an average contemporaneous correlation with all other
crisis of 26 percent that locate it in the 50th percentile.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_LowIncome_footnote.tex"))


# country examples
ctries=c("ARG","FRA","USA","MEX","SWE","LKA","ESP","GRC","JPN")
severity_ctries=lapply(ctries,function(x){
  dt=severity %>% filter(ISO3_Code==x)
  dt=dt %>%  mutate(probability=round(probability,2),
                    priority=round(priority,2),
                    persistence=round(persistence,2),
                    complexity=round(complexity,2),
                    probability_rank=round(probability_rank,2),
                    priority_rank=round(priority_rank,2),
                    persistence_rank=round(persistence_rank,2),
                    complexity_rank=round(complexity_rank,2),
                    area=round(area,2)) %>% arrange(-area) %>%
   arrange(-area) %>% dplyr::select(-ISO3_Code)%>%
    dplyr::select(crisis=shocks,probability,rank=probability_rank,
                  priority,rank=priority_rank,
                  persistence,rank=persistence_rank,
                  complexity,rank=complexity_rank,
                  area)
  
  stargazer::stargazer(title=paste0("Severity:",x)
                       , dt
                       , type="latex"
                       , digits=2
                       , no.space=T
                       , align=T
                       , summary=F
                       , rownames=T
                       , table.placement = "H"
                       , column.sep.width="3pt"
                       , font.size = "footnotesize"
                       , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_",x,".tex")
  )
  dt
})


## worst performing episodes

ctries=ctry_groups %>% filter(Income_group==" High income" & !iso3c %in% c("ATG","BHS","BHR","BRB","BRN","KWT","MAC",
                                                                           "MLT","OMN","PLW","PAN","PRI","QAT","SMR",
                                                                           "SAU","SYC","SGP","KNA","TTO","ARE"))
ctries=ctries$iso3c
top_proba_highIncome=severity %>%
  filter(ISO3_Code %in% ctries) %>% arrange(-probability_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,probability) %>% mutate(probability=round(probability,2)) %>%
  arrange(-probability) %>%
  filter(probability<1)
top_proba_highIncome=top_proba_highIncome[1:10,]
top_proba_highIncome=t(top_proba_highIncome)
top_proba_highIncome=cbind(c("","","Proba."),top_proba_highIncome)

top_priority_highIncome=severity %>% filter(ISO3_Code %in% ctries) %>% arrange(-priority_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,priority) %>% mutate(priority=round(priority,2)) %>%
  arrange(-priority)
top_priority_highIncome=top_priority_highIncome[1:10,]
top_priority_highIncome=t(top_priority_highIncome)
top_priority_highIncome=cbind(c("","","Prior."),top_priority_highIncome)

top_persistence_highIncome=severity %>% filter(ISO3_Code %in% ctries) %>% arrange(-persistence_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,persistence) %>% mutate(persistence=round(persistence,2)) %>%
  arrange(-persistence)
top_persistence_highIncome=top_persistence_highIncome[1:10,]
top_persistence_highIncome=t(top_persistence_highIncome)
top_persistence_highIncome=cbind(c("","","Persist."),top_persistence_highIncome)

top_complex_highIncome=severity %>% filter(ISO3_Code %in% ctries) %>% arrange(-complexity_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,complexity) %>% mutate(complexity=round(complexity,2)) %>%
  arrange(-complexity)
top_complex_highIncome=top_complex_highIncome[1:10,]
top_complex_highIncome=t(top_complex_highIncome)
top_complex_highIncome=cbind(c("","","Complex."),top_complex_highIncome)


top_severity=rbind(top_proba_highIncome,
                   rep("",11),
                   top_priority_highIncome,
                   rep("",11),
                   top_persistence_highIncome,
                   rep("",11),
                   top_complex_highIncome)
colnames(top_severity)=c("Crisis",1:10)

stargazer::stargazer(title="Severity: most severe peformence among high income countries"
                     , top_severity
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=F
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="0pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_worst_episodes.tex")
)


footnote=c("The table displays the top 10 worst performing countries in each severity dimensions. The highest probability
           of occurence is contagion crisis in Estonia with a probability of 96 percent. Sovereign crisis in Croatia and Argentina
           have being allocated the highest priority with a average close to 50 percent of the total length of reports. The longuest
           episodes are contagion in Estonia (an average of 20 years), sovereign default in Poland (12 years), and sovereign crisis
           in Uruguay (9.2 years). The more complex crisis are Economic recession, World outcome and sovereign crisis in Canada, with an average
           correlation with other crisis of 0.42. The table exclude from the sample of High income small countries 
           (ATG,BHS,BHR,BRB,BRN,KWT,MAC,MLT,OMN,PLW,PAN,PRI,QAT,SMR,SAU,SYC,SGP,KNA,TTO,ARE)")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_Worst_episodes_footnote.tex"))




# very high proba countries

excl= c("ATG","BHS","BHR","BRB","BRN","KWT","MAC","MLT","OMN","PLW","PAN","PRI","QAT","SMR","SAU","SYC","SGP","KNA","TTO","ARE")
ctries=mydata %>% filter(year<1990 & !ISO3_Code %in% excl)
ctries=mydata %>% filter(!ISO3_Code %in% excl)
ctries=ctries$ISO3_Code %>% unique()




top_events=function(severity,var,percentile=0.95,direction="higher",path=NULL){
  
  top_events=severity %>% ungroup() %>%
    dplyr::select(ISO3_Code,crisis=shocks,var) %>% group_by(crisis) %>%
    mutate(myvar=round(get(var),2),
           perc_threshold=quantile(myvar,percentile)) %>%
    arrange(-myvar)
  
  if(direction=="lower"){
    top_events=top_events %>% filter(myvar<perc_threshold)
  }else {
    top_events=top_events %>% filter(myvar>perc_threshold)
  }
    
  
  top_events=top_events %>% group_by(crisis) %>% summarize(!!paste0("p",percentile,".threshold"):=unique(perc_threshold),
                                                                     !!paste0("Countries with highest ",var):=paste(paste0(ISO3_Code,"(",myvar,")"),collapse=", "))
   
  
  if(!is.null(path)){
    stargazer::stargazer(title=paste0(var,": Tail countries")
                         , top_events
                         , type="latex"
                         , digits=2
                         , no.space=T
                         , align=F
                         , summary=F
                         , rownames=F
                         , table.placement = "H"
                         , column.sep.width="0pt"
                         , font.size = "footnotesize"
                         , out=path)
  }
  return(top_events) 
}

top_events(severity %>% filter(ISO3_Code %in% ctries),"probability",path=paste0(path_data_directory,"/output/figures/severity/","probability","_worst_episodes.tex")) 
top_events(severity %>% filter(ISO3_Code %in% ctries),"persistence",path=paste0(path_data_directory,"/output/figures/severity/","persistence","_worst_episodes.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"priority",path=paste0(path_data_directory,"/output/figures/severity/","priority","_worst_episodes.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"complexity",path=paste0(path_data_directory,"/output/figures/severity/","complexity","_worst_episodes.tex"))


ctries=ctry_groups %>% filter(Income_group==" High income" & !iso3c %in% c("ATG","BHS","BHR","BRB","BRN","KWT","MAC",
                                                                           "MLT","OMN","PLW","PAN","PRI","QAT","SMR",
                                                                           "SAU","SYC","SGP","KNA","TTO","ARE"))
ctries=ctries$iso3c %>% unique()

top_events(severity %>% filter(ISO3_Code %in% ctries),"probability",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","probability","_worst_episodes_HighIncome.tex")) 
top_events(severity %>% filter(ISO3_Code %in% ctries),"persistence",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","persistence","_worst_episodes_HighIncome.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"priority",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","priority","_worst_episodes_HighIncome.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"complexity",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","complexity","_worst_episodes_HighIncome.tex"))



top_events(severity %>% filter(ISO3_Code %in% ctries),"probability",percentile=0.1,direction="lower")
top_events(severity %>% filter(ISO3_Code %in% ctries),"persistence",percentile=0.2,direction="lower")
top_events(severity %>% filter(ISO3_Code %in% ctries),"priority",percentile=0.1,direction="lower")
top_events(severity %>% filter(ISO3_Code %in% ctries),"complexity",percentile=0.1,direction="lower")



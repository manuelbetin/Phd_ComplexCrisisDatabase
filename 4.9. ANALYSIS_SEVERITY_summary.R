


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
    summarise_at(vars(shocks),mean,na.rm=T) %>%
    gather(key="shocks") %>% dplyr::rename(probability=value)
  
  cond_mean=function(x){
    mean(ifelse(x<lowerbound,NA,x),na.rm=T)
  }
  
  intensity=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    filter(ISO3_Code%in%ctry& year>=1945) %>% ungroup() %>%
    summarise_at(vars(shocks),cond_mean) %>%
    gather(key="shocks")%>% dplyr::rename(intensity=value) %>%
    mutate(intensity=as.numeric(intensity),
           priority=intensity/sum(intensity,na.rm=T)) %>% dplyr::select(-intensity)
  

  myduration=get_duration(mydata %>% filter(ISO3_Code%in%ctry),shocks)
  myduration=myduration %>%
    group_by(shocks) %>%
    summarize(persistence=mean(duration,na.rm=T) %>% round(.,2)) %>%
    arrange(-persistence) %>% filter(!is.na(shocks))
 
  mycomplexity=mydata  %>%
    filter(ISO3_Code%in%ctry& year>=1945) %>%
    ungroup() %>%
    dplyr::select(shocks) %>% na.omit() %>%
     cor() %>% data.frame()
  
  mycomplexity[is.na(mycomplexity)]=0
  
  mycomplexity=mycomplexity %>% summarise_at(vars(shocks),mean,na.rm=T) %>%
    gather(key="shocks",value="complexity")
  
  
  summary_crisis=probability %>% left_join(intensity,by="shocks")
  summary_crisis=summary_crisis %>% left_join(myduration,by="shocks")
  summary_crisis=summary_crisis %>% left_join(mycomplexity,by="shocks")
  
  summary_crisis[is.na(summary_crisis)]=0
  
  summary_crisis=summary_crisis %>% mutate(probability_rank=rank(probability),
                                           priority_rank=rank(priority),
                                           persistence_rank=rank(persistence),
                                           complexity_rank=rank(complexity),
                                           area=probability_rank*priority_rank*persistence_rank*complexity_rank) %>%
    arrange(-area)
  
  return(summary_crisis)
}

ctries=mydata$ISO3_Code %>% unique()
severity=severity_summary_table(mydata,ctries,shocks)

severity=severity %>% mutate(probability=round(probability,2),
                             priority=round(priority,2),
                             persistence=round(persistence,2),
                             complexity=round(complexity,2)) %>%
  dplyr::select(shocks,probability,rank=probability_rank,priority,rank=priority_rank,persistence,
                rank=persistence_rank,complexity,rank=complexity_rank,area)

stargazer::stargazer(title="Severity: summary table"
                     , severity
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


#high income

ctries=ctry_groups %>% filter(Income_group==" High income")
ctries=ctries$iso3c
severity=severity_summary_table(mydata,ctries,shocks)

severity=severity %>% mutate(probability=round(probability,2),
                             priority=round(priority,2),
                             persistence=round(persistence,2),
                             complexity=round(complexity,2)) %>%
  dplyr::select(shocks,probability,rank=probability_rank,priority,rank=priority_rank,persistence,
                rank=persistence_rank,complexity,rank=complexity_rank,area)

stargazer::stargazer(title="Severity: summary table, High Income"
                     , severity
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

#middle income

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
ctries=ctries$iso3c
severity=severity_summary_table(mydata,ctries,shocks)

severity=severity %>% mutate(probability=round(probability,2),
                             priority=round(priority,2),
                             persistence=round(persistence,2),
                             complexity=round(complexity,2)) %>%
  dplyr::select(shocks,probability,rank=probability_rank,priority,rank=priority_rank,persistence,
                rank=persistence_rank,complexity,rank=complexity_rank,area)

stargazer::stargazer(title="Severity: summary table, Middle Income"
                     , severity
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


#Low income

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
ctries=ctries$iso3c
severity=severity_summary_table(mydata,ctries,shocks)

severity=severity %>% mutate(probability=round(probability,2),
                             priority=round(priority,2),
                             persistence=round(persistence,2),
                             complexity=round(complexity,2)) %>%
  dplyr::select(shocks,probability,rank=probability_rank,priority,rank=priority_rank,persistence,
                rank=persistence_rank,complexity,rank=complexity_rank,area)

stargazer::stargazer(title="Severity: summary table, Low Income"
                     , severity
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




# country examples
ctries=c("ARG","FRA","USA","MEX","SWE","LKA","ESP","GRC","JPN")
severity_ctries=lapply(ctries,function(x){
  dt=severity_summary_table(mydata,x,shocks)
  dt=dt  %>% mutate(probability=round(probability,2),
                    priority=round(priority,2),
                    persistence=round(persistence,2),
                    complexity=round(complexity,2)) %>%
    dplyr::select(shocks,probability,rank=probability_rank,priority,rank=priority_rank,persistence,
                  rank=persistence_rank,complexity,rank=complexity_rank,area)
  stargazer::stargazer(title=paste0("Severity: summary table ",x)
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








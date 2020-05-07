


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

severity_summary_table=function(mydata,ctry="FRA",shocks){
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
    filter(year >= period_range[1] & year<=period_range[2]) %>% ungroup()%>%
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
           priority=intensity/sum(intensity,na.rm=T))
  
  duration=get_duration(mydata %>% filter(ISO3_Code%in%ctry),shocks)
  duration=duration %>%
    group_by(shocks,statistic) %>%
    summarize(mean=mean(stat,na.rm=T)) %>%
    spread(key=statistic,value=mean) %>%
    ungroup()
  colnames(duration)=c("shocks","p25","p75","max","mean","median","min")
  duration=duration%>% ungroup() %>%
    dplyr::select(shocks,duration=mean) %>%
    mutate(shocks=str_remove_all(shocks," "))
  
  summary_crisis=probability %>% left_join(intensity,by="shocks")
  summary_crisis=summary_crisis %>% left_join(duration,by="shocks")
  
  summary_crisis=summary_crisis %>% mutate(probability_rank=rank(probability),
                                           intensity_rank=rank(intensity),
                                           duration_rank=rank(duration),
                                           area=probability_rank*intensity_rank*duration_rank) %>%
    arrange(-area)
  
  return(summary_crisis)
}

ctries=mydata$ISO3_Code %>% unique()
severity=severity_summary_table(mydata,ctries)

severity=severity %>% mutate(probability=round(probability,2),
                             priority=round(priority,2),
                             duration=round(duration,2)) %>% 
  dplyr::select(-intensity) %>% rename(persistence=duration)

stargazer::stargazer(title="Severity: summary table"
                     , severity
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=T
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary.tex")
)

ctries=c("ARG","FRA","USA","MEX","SWE")
severity_ctries=lapply(ctries,function(x){
  dt=severity_summary_table(mydata,x)
  dt=dt %>% mutate(probability=round(probability,2),
                   priority=round(priority,2),
                   duration=round(duration,2)) %>% 
    dplyr::select(-intensity) %>% rename(persistence=duration)
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








######## Description: the script generates graph for the time series of some less standard events

# Average data over year:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)

library(forcats)


# Probability of events  ####
get_probability=function(mydata,shocks,period_range=c(1960,2019),lowerbound=0,path=NULL){
  #' @title plot event study of crisis by country
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param ctry the country to which display the figure
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  
  
 myfig=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    mutate_at(vars(shocks), get_prob) %>% 
    filter(year >= period_range[1] & year<=period_range[2]) %>% ungroup()%>%
    summarise_at(vars(shocks),mean,na.rm=T) %>%
    gather(key="shock") %>% mutate(shock = fct_reorder(shock,value))%>%
    ggplot() +
    geom_bar(stat="identity",aes(x=shock,y=value),fill="darkgrey",col = "black",alpha=0.6) +
    theme_bw()+
    labs(y="Share of countries (%)",
         x=NULL,
         title=NULL)+
   lims(y=c(0,1))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =11,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="none")
  
    if(!is.null(path)){
      myfig + ggsave(filename=paste0("Probability_shocks_",period_range[1],"-",period_range[2],".png"),device = 'png',path=path)
    }else{
      myfig
    }
}

get_probability(mydata,shocks,period_range=c(1960,1980),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")
get_probability(mydata,shocks,period_range=c(1980,2000),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")
get_probability(mydata,shocks,period_range=c(2000,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")
get_probability(mydata,shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")

ctries=ctry_groups %>% filter(Income_group==" High income")
get_probability(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/HighIncome")

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_probability(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/MiddleIncome")

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_probability(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/LowIncome")


# Intensity of events ####

get_intensity=function(mydata,shocks,lowerbound=0,path=NULL){
  #' @title plot event study of crisis by country
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  # buckets=list("2013-2020"=c(2013,2020),
  #              "2003-2013"=c(2003,2013),
  #              "1992-2003"=c(1992,2003),
  #              "1976-1992"=c(1976,1992),
  #              "1950-1976"=c(1950,1976))
  # 
  buckets=list("1950-2020"=c(1950,2020))
  
 data= lapply(buckets,function(x){
    data1=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
      filter(year >= x[1] & year<=x[2]) %>% ungroup()%>%
      summarise_at(vars(shocks),cond_mean) %>%
      gather(key="shock") %>% mutate(shock = fct_reorder(shock,value))%>%
      mutate(value=value/sum(value,na.rm=T),
             bucket=paste0(x[1],"-",x[2])) #%>%
  })
 data=do.call(rbind,data)
  
  myfig=ggplot(data) +
    geom_bar(stat="identity",aes(x=shock,y=value,group=bucket,fill=bucket),col = "black",alpha=0.9) +
    theme_bw()+
    labs(y="% of total shock",
         x=NULL,
         title=NULL)+
    #lims(y=c(0,1))+
    scale_fill_grey()+ 
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =11,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="right",
          legend.title = element_blank())
  
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("Intensity_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}

get_intensity(mydata,shocks,path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Intensity/All")

ctries=ctry_groups %>% filter(Income_group==" High income")
get_intensity(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Intensity/HighIncome")

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_intensity(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Intensity/MiddleIncome")

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_intensity(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Intensity/LowIncome")


# Time series of priority ####

get_first_priority=function(mydata,shocks,lowerbound=0,path=NULL){
  #' @title plot time series with first priority
  #' @description ggplot figure showing the first priority for 
  #' each year
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  
  dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    group_by(year)%>%
    summarise_at(vars(shocks),cond_mean) %>%
    gather(key="shock",value="value",-c("year")) %>% ungroup() %>%
    group_by(year) %>%
    mutate(max_value=max(value,na.rm=T)) %>%
    mutate(tot_value=round(max_value/sum(value,na.rm=T)*100,2)) %>%
    filter(max_value==value) %>%
    arrange(-year) %>% filter(year>1945) %>%
    mutate(shock_1=ifelse(year %% 2==1,"",shock),
           mylabel1=ifelse(year %% 2==1,"",paste0(gsub("_"," ",shock))),
           mylabel2=ifelse(year %% 2==0,"",paste0(gsub("_"," ",shock))),
           myprioritylabel1=ifelse(year %% 2==1,"",paste0(" (",tot_value," %)")),
           myprioritylabel2=ifelse(year %% 2==0,"",paste0(" (",tot_value," %)")),
           myyearlabel1=ifelse(year %% 2==1,"",as.character(year)),
           myyearlabel2=ifelse(year %% 2==0,"",as.character(year)),
           year1=ifelse(year %% 2==1,NA,20),
           year2=ifelse(year %% 2==0,NA,-20)) %>% arrange(tot_value)
  
  
  myfig=ggplot(dt)+
    geom_text(aes(x=year,y=year1,label=mylabel1,
                  vjust="-0.2",hjust="left",color=mylabel1),
              angle=60,size=3.5)+
    geom_text(aes(x=year,y=year2,label=mylabel2,
                  vjust="left",hjust="right",color=mylabel2),
              angle=60,size=3.5)+
    geom_text(aes(x=year,y=0,label=myyearlabel1,
                  vjust=-0.4,hjust=-0.7),color="black",
              angle=90,size=2.5)+
    geom_text(aes(x=year,y=0,label=myyearlabel2,
                  vjust=-0.4,hjust=1.5),color="black",
              angle=90,size=2.5)+
    #geom_hline(aes(yintercept=0))+
    geom_line(aes(x=year,y=0),color="darkgrey")+
    geom_col(aes(x=year,y=year1-1),width = 0.05,color="darkgrey")+
    geom_col(aes(x=year,y=year2+1),width = 0.05,color="darkgrey")+
    scale_color_grey()+
    theme_minimal()+
    labs(y=NULL,
         x=NULL,
         title=NULL)+
    ylim(c(-70,70))+
    xlim(c(1940,2030))+
    scale_fill_grey()+
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size=11),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_blank(),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="none",
          legend.title = element_blank())
  
  myfig
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("TS_priority_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}  

#"Sovereign_default",'Natural_disaster',
shocks=c("Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

get_first_priority(mydata,shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Priority")

ctries=ctry_groups %>% filter(Income_group==" High income")
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Priority/HighIncome")
  
ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Priority/MiddleIncome")

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Priority/LowIncome")


get_priority_table=function(mydata,shocks,lowerbound=0,path=NULL){
  
  #' @title table with summary statistics of priorities
  #' @description table with summary statistics of priorities 
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return dataframe 
  #' @author Manuel Betin
  #' @export
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>% ungroup() %>%
    group_by(ISO3_Code)%>%
    summarise_at(vars(shocks),cond_mean) %>%
    gather(key="shock",value="value",-c("ISO3_Code")) %>% ungroup() %>%
    group_by(ISO3_Code) %>%
    mutate(tot=sum(value,na.rm=T),
           weight=value/tot*100) %>%
    group_by(shock) %>%
    summarize(mean=mean(weight,na.rm=T)%>% round(1),
              sd=sd(weight,na.rm=T)%>% round(1),
              p.10=quantile(weight,p=0.10,na.rm=T)%>% round(1),
              p.50=quantile(weight,p=0.50,na.rm=T)%>% round(1),
              p.80=quantile(weight,p=0.8,na.rm=T)%>% round(1),
              p.95=quantile(weight,p=0.95,na.rm=T)%>% round(1)) %>% arrange(-mean) 
  
    return(dt)
}

shocks=c("Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

stargazer::stargazer(title="Intensity: Summary of priorities"
                     , get_priority_table(mydata,shocks)
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out="../Betin_Collodel/2. Text mining IMF_data/output/figures/Priority/Summary_priority.tex"
)

# Duration of events ####

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

get_duration=function(mydata,shocks){
  ctries=mydata$ISO3_Code %>% unique()
  all=lapply(ctries,function(ctry){
   # var="Sovereign_default"
    dt=lapply(shocks,function(var){
      dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>% 
        filter(ISO3_Code==ctry & year>=1945) %>% ungroup() %>%
        group_by(year)%>%
        summarise_at(vars(shocks),cond_mean) %>%
        mutate_at(vars(shocks),get_prob)%>% dplyr::select(year,var) %>%
        mutate(var:=ifelse(is.na(get(var)),0,get(var))) %>%
        arrange(year) 
      dt=dt %>% na.omit()
      epi=0
      dt$episode=0
      count=0
      for(i in 1:(dim(dt)[1]-1)){
        if(i+count<dim(dt)[1]-1){
          i=i+count
          if(dt[[i,var]]>0){
            count=1
            epi=epi+1
            dt[i,"episode"]=epi
            dt[i,var]=count
            if(i+count<dim(dt)[1]-1){
              while(dt[[ifelse(i+count>=dim(dt)[1],dim(dt)[1],i+count),var]]==1 & count<20){
                count=count+1
                dt[i,var]=count
                dt[i,"episode"]=epi
              }
            }
          }
        }
      }
      dt=dt %>% filter(episode>0)
      dt=dt[,1:2]
      summary(dt[,var],na.rm=T) %>% data.frame()
    })
    names(dt)=shocks
    dt=do.call(rbind,dt) %>% dplyr::select(shocks=Var2,stat=Freq) %>%
      mutate(statistic=substr(stat,1,7),
                   stat=as.numeric(substr(stat,9,13)))
    dt$ISO3_Code=ctry
    print(ctry)
    dt
  })
  all=do.call(rbind,all)
  all=data.frame(all)
  return(all)
}

get_duration_fig=function(mydata,shocks,lowerbound=0,path=NULL){
  #' @title barplot of duration of crisis
  #' @description ggplot figure showing the average duration 
  #' of episodes
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  avg_duration=get_duration(mydata,shocks)
  
  avg_dur=avg_duration %>%
    group_by(shocks,statistic) %>%
    summarize(mean=mean(stat,na.rm=T)) %>%
    spread(key=statistic,value=mean) %>%
    ungroup()
  colnames(avg_dur)=c("shocks","p25","p75","max","mean","median","min")
  avg_dur=avg_dur%>% ungroup() %>% mutate(shocks = fct_reorder(shocks,mean)) 

  myfig=ggplot(avg_dur)+
    geom_errorbar(aes(x=shocks,ymin = min, ymax = max),color="grey")+
    #geom_bar(stat="identity",aes(x=shocks,y=mean),fill="darkgrey",col = "black",alpha=0.6)+
    geom_point(aes(x=shocks,y=mean),fill = "red",alpha=0.6,shape=21)+
    geom_text(aes(x=shocks,y=max,label=round(max,1)),color = "grey",alpha=1,vjust=-1)+
    geom_text(aes(x=shocks,y=min,label=round(min,1)),color = "grey",alpha=1,vjust=1)+
    geom_text(aes(x=shocks,y=mean,label=round(mean,1)),color = "black",alpha=1,vjust=-1)+
    theme_bw()+
    labs(y="N. years",
         x=NULL,
         title=NULL)+
    lims(y=c(-1,max(avg_dur$max)+5))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =11,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="bottom")
  
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("duration_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}  

get_duration_fig(mydata,shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Duration")

ctries=ctry_groups %>% filter(Income_group==" High income")
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Duration/HighIncome")

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Duration/MiddleIncome")

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Duration/LowIncome")


# summary table with all dimensions

severity_summary_table=function(mydata,ctry="FRA"){
  
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
                     , out="../Betin_Collodel/2. Text mining IMF_data/output/figures/severity/Severity_summary.tex"
)


a=severity_summary_table(mydata,"ARG")
a=severity_summary_table(mydata,"FRA")
a=severity_summary_table(mydata,"USA")

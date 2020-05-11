#' @title summary figures of intensity
#' @description produce the barplot comparison
#' the intensity of each crisis
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder Intensity


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"


# Average data over year:

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
  
  buckets=list("2013-2020"=c(2013,2020),
               "2003-2013"=c(2003,2013),
               "1992-2003"=c(1992,2003),
               "1976-1992"=c(1976,1992),
               "1950-1976"=c(1950,1976))
  
  #buckets=list("1950-2020"=c(1950,2020))
  
  data= lapply(buckets,function(x){
    data1=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
      filter(year >= x[1] & year<=x[2]) %>% ungroup()%>%
      summarise_at(vars(shocks),cond_mean) %>%
      gather(key="shock") %>% mutate(shock=gsub("_"," ",shock),
                                     shock=ifelse(shock=="Balance payment crisis","BOP crisis",
                                                  ifelse(shock=="Currency crisis severe","Currency crisis",shock)),
                                     shock = fct_reorder(shock,value))%>%
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
          axis.text.x = element_text(size =15,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
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

get_intensity(mydata,shocks,path=paste0(path_data_directory,"/output/figures/Intensity/All"))

ctries=ctry_groups %>% filter(Income_group==" High income")
get_intensity(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,path=paste0(path_data_directory,"/output/figures/Intensity/HighIncome"))

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_intensity(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,path=paste0(path_data_directory,"/output/figures/Intensity/MiddleIncome"))

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_intensity(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,path=paste0(path_data_directory,"/output/figures/Intensity/LowIncome"))


footnote=c("The figures display the relative priority of each crisis for each bucket of periods. It is computed as 
           the average tf.idf divided by the sum of the tf.idf of the crisis category. This measure provide a measure
           of the relative importance of a given crisis conditional of its occurence and give a proxy for the average
           share of the reports allocated to the given crisis.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Duration/Duration_shock_footnote.tex"))


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

#excludes Sovereign_default
shocks=c("Soft_recession","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

get_first_priority(mydata,shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority"))

ctries=ctry_groups %>% filter(Income_group==" High income")
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority/HighIncome"))

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority/MiddleIncome"))

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority/LowIncome"))


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

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
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
                     , out=paste0(path_data_directory,"/output/figures/Priority/Summary_priority.tex")
                     
)
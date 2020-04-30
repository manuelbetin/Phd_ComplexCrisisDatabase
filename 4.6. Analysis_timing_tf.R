######## Description: the script generates graph for the time series of some less standard events

# Average data over year:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

library(forcats)


#Probability of events 
get_timeserie=function(mydata,shocks,period_range=c(1960,2019),lowerbound=0,path=NULL){
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


get_timeserie(mydata,shocks,period_range=c(1960,1980),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")
get_timeserie(mydata,shocks,period_range=c(1980,2000),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")
get_timeserie(mydata,shocks,period_range=c(2000,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")
get_timeserie(mydata,shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")

ctries=ctry_groups %>% filter(Income_group==" High income")
get_timeserie(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/HighIncome")

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_timeserie(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/MiddleIncome")

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_timeserie(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/LowIncome")



#Intensity of events

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
  
 data= lapply(buckets,function(x){
    data1=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
      filter(year >= x[1] & year<=x[2]) %>% ungroup()%>%
      summarise_at(vars(shocks),cond_mean) %>%
      gather(key="shock") %>% mutate(shock = fct_reorder(shock,value))%>%
      mutate(value=value/sum(value),
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



get_first_priority=function(mydata,ctries=NULL,shocks,lowerbound=0,path=NULL){
  #' @title plot time series of priorities shock
  #' @describeIn ggplot figure showing the first priority for 
  #' each year
  #' @param mydata the tf.idf database 
  #' @param ctry the country of interest
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  if(!is.null(ctries)){
    cond_mean=function(x){
      mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
    }
    mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
      group_by(ISO3_Code,year)%>%
      summarise_at(vars(shocks),cond_mean) %>%
      gather(key="shock",value="value",-c("ISO3_Code","year")) %>% ungroup() %>%
      group_by(ISO3_Code,year) %>%
      mutate(max_value=max(value,na.rm=T),
             top_x = value %in% head(sort(value, decreasing = TRUE), 2)) %>%
      mutate(tot_value=round(max_value/sum(value,na.rm=T)*100,2)) %>%
      filter(max_value==value) %>%
      filter(ISO3_Code%in%ctries) %>% arrange(-year) %>%
      ggplot()+
      # geom_text(aes(x=year,y=10,label=paste0(year,": ",gsub("_"," ",shock)," (",tot_value," % of total shocks)"),
      #               vjust=-"left",hjust="left",group=ISO3_Code),
      #           angle=0,size=2)+
      geom_text(aes(x=year,y=mean(tot_value),label=paste0(gsub("_"," ",shock)," (",tot_value," %)"),
                    vjust=-"left",hjust="left"),
                angle=0,size=4)+
      
      theme_minimal()+
      labs(y=NULL,
           x=NULL,
           title=NULL)+
      scale_fill_grey()+ 
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size=11),
            axis.text.y = element_text(size=11),
            plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5),
            legend.position="right",
            legend.title = element_blank())  +
      coord_flip()
  }else{
    cond_mean=function(x){
      mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
    }
    myfig=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
      group_by(year)%>%
      summarise_at(vars(shocks),cond_mean) %>%
      gather(key="shock",value="value",-c("year")) %>% ungroup() %>%
      group_by(year) %>%
      mutate(max_value=max(value,na.rm=T)) %>%
      mutate(tot_value=round(max_value/sum(value,na.rm=T)*100,2)) %>%
      filter(max_value==value) %>%
      arrange(-year) %>% filter(year>1945) %>%
      ggplot()+
      # geom_text(stat="identity",aes(x=year,y=10,label=paste0(year,": ",gsub("_"," ",shock)," (",tot_value," % of total shocks)"),
      #              hjust="left",vjust=0.4),
      #           angle=45,size=2)+
      geom_text(aes(x=year,y=mean(tot_value),label=paste0(year,": ",gsub("_"," ",shock)," (",tot_value," %)"),
                    vjust="left",hjust="left"),
                angle=0,size=2.5)+
      geom_point(aes(x=year,y=tot_value),size=1,color="black")+
      theme_minimal()+
      labs(y=NULL,
           x=NULL,
           title=NULL)+
      scale_fill_grey()+ 
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.x = element_text(size=11),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size=11),
            axis.text.y = element_blank(),
            plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5),
            legend.position="right",
            legend.title = element_blank()) +
      coord_flip()
  }
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("Intensity_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}  
  

shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
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



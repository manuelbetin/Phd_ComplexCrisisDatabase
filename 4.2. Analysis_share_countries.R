######## Description: the script generates graph for the probability part 
######## Graph 1: share of countries with currency crisis, Graph 2: correlation with inflation crisis
######## To do: automate correlation in graph 2

# Average by year:
mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

get_share_country=function(mydata,shocks,ymin=0,ymax=1,rollmean=5,lowerbound=0,dotpercentile=0.99,path=NULL){
  #' @title plot share of countries experiencing the shocks
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param ymin minimum for y axis
  #' @param ymax maximum for x axis
  #' @param rollmean the number of years to consider from the moving 
  #' average smoothing
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param dotpercentile percentile above which the year is highlighted by
  #' a red dot 
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  

  if(any(shocks %in% names(lexicon()))){
  fig=lapply(shocks,function(x){
    
    myfig=mydata %>% 
      mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
      ungroup() %>% 
      # group_by(ISO3_Code)%>%
      # mutate(N=n())%>% ungroup() %>%
      group_by(year) %>% 
      filter(year >= 1946) %>% 
      summarise(var = mean(get(x), na.rm = T),
                N=n()) %>%
      mutate(var_ma = zoo::rollmean(var, rollmean, align = "center", fill = NA))  %>% 
      ungroup()%>%
      mutate(max_var=ifelse(var_ma>=quantile(var_ma,na.rm=T,p=dotpercentile),var_ma,NA)) %>%
      ggplot(aes(year, col="share")) +
      geom_bar(stat="identity",aes(y = var),fill ="darkgrey",color="darkgrey",width = 0.5,position = position_dodge(width = 1),alpha=0.2) +
      geom_line(aes(y = var_ma),col ="darkblue",size=1) +
      geom_point(aes(y=max_var),col="red",size=2)+
      geom_text(aes(y=0.95,x=year,label=ifelse(year %in% c(1950,1960,1970,1980,1990,2000,2010,2019),paste0("N=",N),"")),color="black",size=3,angle = 90,alpha=0.9)+
      theme_bw()+
      labs(y="Share of countries (%)",
           x=NULL,
           title=NULL)+
      lims(y=c(ymin,ymax))+
      scale_x_continuous(breaks=seq(1945,2020,5))+ #set y ticks
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(size =11,angle=90),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size=11),
            axis.text.y = element_text(size=11),
            plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5),
            legend.position="none")
    
    if(!is.null(path)){
      myfig + ggsave(filename=paste0("Share_",x,".png"),device = 'png',path=path)
    }else{
      myfig
    }
  })
  names(fig)=shocks
  
  }else{
    warning("please provide a valid name of shock amoung names(lexicon())")
  }
  
}

fig_ctry_shares=get_share_country(mydata,shocks,rollmean = 3,lowerbound = 0.0,
                                  path = "../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")  
names(fig_ctry_shares)=shocks

#selected shocks for low income groups
ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
Share_lowIncome=get_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                  shocks=shocks,
                  rollmean = 3,
                  path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/LowIncome")  

#selected shocks for high income groups
ctries=ctry_groups %>% filter(Income_group==" High income")
Share_HighIncome=get_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                                  shocks=shocks,
                                  rollmean = 3,
                                  path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/HighIncome")  

#selected shocks for middle income groups
ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
Share_UpperMiddleIncome=get_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                                   shocks=shocks,
                                   rollmean = 3,
                                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/MiddleIncome")  


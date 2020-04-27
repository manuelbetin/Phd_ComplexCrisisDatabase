######## Description: the script generates graphs and tables for the probability part.


# Average by year:
mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

# Plots -----

plot_share_country=function(mydata,shocks,ymin=0,ymax=1,rollmean=5,lowerbound=0,dotpercentile=0.99,path=NULL){
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
      geom_text(aes(y=0.95,x=year,label=ifelse(year %in% c(1950,1960,1970,1980,1990,2000,2010,2019),paste0("N=",N),"")),color="black",size=5,angle = 90,alpha=0.9)+
      theme_bw()+
      labs(y="Share of countries (%)",
           x=NULL,
           title=NULL)+
      lims(y=c(ymin,ymax))+
      scale_x_continuous(breaks=seq(1945,2020,5))+ #set y ticks
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(size =14,angle=90),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size=114),
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

fig_ctry_shares=plot_share_country(mydata,shocks,rollmean = 3,lowerbound = 0.0,
                                  path = "../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/All")  
names(fig_ctry_shares)=shocks

#selected shocks for low income groups
ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
Share_lowIncome=plot_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                  shocks=shocks,
                  rollmean = 3,
                  path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/LowIncome")  

#selected shocks for high income groups
ctries=ctry_groups %>% filter(Income_group==" High income")
Share_HighIncome=plot_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                                  shocks=shocks,
                                  rollmean = 3,
                                  path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/HighIncome")  

#selected shocks for middle income groups
ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
Share_UpperMiddleIncome=plot_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                                   shocks=shocks,
                                   rollmean = 3,
                                   path="../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/MiddleIncome")  


# Table with max for every shock and income group: -----


table_share_country=function(mydata,shocks,lowerbound=0,max=0.99,path=NULL){
  #' @title compile table with year of maximum every shock
  #' @describeIn TeX table with year share of countries reach maximum for each shock and associated share
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param max percentile above which to consider maximum
  #' @param path the path of the directory to save the figures
  #' 
  #' @return Invisibly saves tex object or, if path null, dataframe.
  #' @author Umberto collodel
  #' @export
  
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  
  
  if(any(shocks %in% names(lexicon()))){
    list_shares=lapply(shocks,function(x){
      mydata %>% 
        mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
        ungroup() %>% 
        # group_by(ISO3_Code)%>%
        # mutate(N=n())%>% ungroup() %>%
        group_by(year) %>% 
        filter(year >= 1946) %>% 
        summarise(var = round(mean(get(x), na.rm = T),2)) %>% 
        ungroup()%>%
        mutate(max_var=ifelse(var>=quantile(var,na.rm=T,p=max),var,NA)) 
      })
  }
  
  table_max <- list_shares %>% 
    map2(shocks, ~ .x %>% select(year, max_var) %>% filter(!is.na(max_var)) %>% mutate(Type_index = .y) %>% select(Type_index,year, max_var)) %>% 
    bind_rows() 
  
  if(!is.null(path)){
    stargazer(table_max, summary = F, out = path)
  }
  else{
  table_max
  }
  
}


# Create a table for every income group: (TO DO: Manu substitute with ctry group function!)

income_groups <- c("High income","Upper middle income","Low income")

classification <- import("../Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData") %>% 
  select(ISO3_Code,Income_group,group) %>% 
  filter(!duplicated(ISO3_Code)) %>% 
  mutate(Income_group = ifelse(Income_group == "Lower middle income","Low income",Income_group))


table_income_group <- income_groups %>% 
  map(function(x){
    ctries <- classification %>% filter(Income_group == x)
    table_share_country(mydata %>% filter(ISO3_Code %in% ctries$ISO3_Code), shocks) %>% 
      mutate(Income_group = x)}
    ) 

# Final table:

table_income_group %>% 
  bind_rows() %>% 
  arrange(Type_index, Income_group) %>% 
  select(Type_index, Income_group, year, max_var) %>%
  mutate(Type_index = str_replace_all(Type_index,"_"," ")) %>% 
  rename(`Income Group` = Income_group, Year = year, `Share of countries` = max_var, `Category` = Type_index) %>% 
  stargazer(summary = F, rownames = F, out = "../Betin_Collodel/2. Text mining IMF_data/output/tables/Probability/max_share_detail.tex")
  
?stargazer


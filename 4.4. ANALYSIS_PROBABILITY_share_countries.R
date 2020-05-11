#' @title figures with share of countries in crisis
#' @description produce the figures showing the time 
#' series of the proportion of countries experiencing
#' the selected crisis across years
#' @author Manuel Betin, Umberto Collodel
#' @return figures and tables in the folder Probability

path_data_directory="../Betin_Collodel/2. Text mining IMF_data"


# Average by year:
mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

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
            axis.text.x = element_text(size =15,angle=90),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size=15),
            axis.text.y = element_text(size=15),
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
  fig
  }else{
    warning("please provide a valid name of shock amoung names(lexicon())")
  }

}

fig_ctry_shares=plot_share_country(mydata,shocks,rollmean = 3,lowerbound = 0.0,
                                  path = paste0(path_data_directory,"/output/figures/Probability/All"))
names(fig_ctry_shares)=shocks


#Economic outcomes

fig_ctry_shares$Severe_recession
fig_ctry_shares$World_outcomes
fig_ctry_shares$Soft_recession

footnote=c("Grey bars denote the yearly proportion of countries experiencing a severe recession (panel a), and a contagion
from global crisis (panel b). The blue line denote the 3 years moving average and the red dot the highest 
value over the period. Between 1945 and 2019 there were five spike denoting wide spread recession around the
world: 1976, 1984,1994,1999, 2009. Among those only 1975, 1984, 1999 and 2009-2010 are reference as international crisis
an a large part of the world. 2009 stands out as the more wide spread crisis with 75 percent of countries impacted by the
2009 global recession, while 50 percent of countries are reported to experience a severe recessions in those years.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Probability/All/Probability_share_ctry_economic_outcomes_footnote.tex"))

#Currency and trade crisis

fig_ctry_shares$Currency_crisis_severe
fig_ctry_shares$Trade_crisis
fig_ctry_shares$Balance_payment_crisis

footnote=c("Grey bars denote the yearly proportion of countries experiencing a currency crisis (panel a), and a trade
crisis (panel b). The blue line denote the 3 years moving average and the red dot the highest 
value over the period. The period 1945-1980 shows very little currency or trade tension with less than 5 percent of countries
experiencing such event on average every year. On constrast 1981 starts a period of increasing number of 
currency crisis peaking in 1986 with more than 25 percent of countries suffering a currency crisis. This level has since
then follow a very slow reduction at an average of around 10 percent since 2004. Trade crisis display a very similar
pattern from 1945 to 2017, A very calm period during 1945-1982 is followed by a sudden peak in 1983 at around 25 percent 
of countries (US/Japan trade war of 1987 shows in 1989) that stabilize up to 1994 to then decerease steadily to less than
5 percent in 2013. 2014 marks the start of the trade tension with a spectacular increase since 2017 triggered 
by The US/China trade tensions.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Probability/All/Probability_share_ctry_Currency_Trade_footnote.tex"))


#Financial fragility

fig_ctry_shares$Financial_crisis
fig_ctry_shares$Contagion
fig_ctry_shares$Expectations
fig_ctry_shares$Banking_crisis


footnote=c("Grey bars denote the yearly proportion of countries experiencing a Financial crisis (panel a), a Banking
crisis (panel b), a reverseal of financial market expectations (panel c) and a financial contagion (panel d).
The blue line denote the 3 years moving average and the red dot the highest value over the period. 
The period 1945-1980 appears a remarquably calm period with almost no financial crisis across the world. Since then 
           several waves of increased the yearly number of financial crisis with a first plateau around 10 percent
           of countries from 1982 to 1993 with a steady increase peaking in 1998 with Asian and Latin American financial
           turmoils. The third wage is sudden an sharpe, between 2008 and 2010 with 75 percent of countries in financial 
           crisis in 2009. Altough the number of financial crisis as reduce it never passed the 50 percent threshold
           with a more suprisingly increase since 2015 culminating in 2017. Regarding the three other measure of financial 
           vulnerability we observe that the increase starting in 1980 occur far less in waves but rather a constant increase 
           culminating in the recent period. Indeed in 20 years the number of countries experience tension and the 
           banking sector has increased from less than 5 percent in 1980 to more than 50 percent in early 2000. This very 
           rapid and steady increase is then follow by a slower, yet increasing trend with spikes in 2003, 2010 and 2017. 
           Almost mirroring the developments of the banking sector the financial contagion display a similar trend with
           spikes in 1991, 1998 and 1999, 2003, 2009, 2014 and 2017. Uncertain expectations on financial markets 
           and fears of market sentiment display the more striking pattern with an almost continuous jump from around
           10 percent of countries concerned in 1980 to almost 90 percent in 2019. In the pre 1980 period and in contrast
           to the other measures two spikes can be noted 1953 due to uncertainties of the Korean War and
           1971-1974 due to the end of Bretton Woods and the first oil shock.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Probability/All/Probability_share_ctry_Financial_fragility_footnote.tex"))


#selected shocks for low income groups
ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
Share_lowIncome=plot_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                  shocks=shocks,
                  rollmean = 3,
                  path=paste0(path_data_directory,"/output/figures/Probability/LowIncome"))  



#selected shocks for high income groups
ctries=ctry_groups %>% filter(Income_group==" High income")
Share_HighIncome=plot_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                                  shocks=shocks,
                                  rollmean = 3,
                                  path=paste0(path_data_directory,"/output/figures/Probability/HighIncome")) 

#selected shocks for middle income groups
ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
Share_UpperMiddleIncome=plot_share_country(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
                                   shocks=shocks,
                                   rollmean = 3,
                                   path=paste0(path_data_directory,"/output/figures/Probability/MiddleIncome"))  


#Epidemic outbreaks

fig_ctry_shares$Epidemics
Share_HighIncome$Epidemics
Share_lowIncome$Epidemics
Share_UpperMiddleIncome$Epidemics


footnote=c("Grey bars denote the yearly proportion of countries experiencing a Epidemics crisis,
           The blue line denote the 3 years moving average and the red dot the highest value over the period. 
           At the global level, the period 1945-1994 display a relatively stable share of countries experiencing
           epidemic pressure with little less than 10 percen on average and is follow by a rapide increase from almost
           zero to close to 50 percent of the world mentionning epidemic concerns. the post 2005 period show a steady decline
           with yet a spike 2015 due to the Ebola outbreak and 2020 with the start of the COVID19 pandemia. Regarding the 
           difference across income group we observe that the COVID19 already stands out as the more wide spread 
           epidemic even we data only available up to march. A few waves are still to mention with a spike in 1953,
           and waves in 1968-1975, 1980-1981, 1991, 1998, and 2002-2005. For middle income countries 2020 will definitely
           be the worst epidemics but is at March 2020 at the level of the spike of 2004 that end a period of steady spreading
           of epidemics from early 1990. Among previous peaks we see one in 1950, and a wave 1968-1975. Low income countries
           is the income group more widely concerned by epidemics with more than 50 percent of low income countries
           experiencing an epidemic problem from 1999 to 2010 and a more recent spike in 2017.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Probability/All/Probability_share_ctry_Epidemics_footnote.tex"))




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

classification <- import(paste0(path_data_directory,"/datasets/comparison/other_data.RData")) %>% 
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
  stargazer(summary = F, rownames = F, out = paste0(path_data_directory,"/output/tables/Probability/max_share_detail.tex"))
  


#' @title summary figures of probability
#' @description produce the barplot comparison
#' the probability of each crisis
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder Probability

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
   filter(year >= period_range[1] & year<=period_range[2]) %>% ungroup()%>%
    mutate_at(vars(shocks), get_prob) %>% 
    summarise_at(vars(shocks),mean,na.rm=T) %>%
    gather(key="shock") %>% mutate(shock = fct_reorder(shock,value))%>%
    ggplot() +
    geom_bar(stat="identity",aes(x=shock,y=value),fill="darkgrey",col = "black",alpha=0.6) +
    geom_text(aes(x=shocks,y=value,label=round(value,2)),color = "grey",alpha=1,vjust=-1)+
    theme_bw()+
    labs(y="Share of countries",
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

get_probability(mydata,shocks,period_range=c(1960,1980),path=paste0(path_data_directory,"/output/figures/Probability/All"))
get_probability(mydata,shocks,period_range=c(1980,2000),path=paste0(path_data_directory,"/output/figures/Probability/All"))
get_probability(mydata,shocks,period_range=c(2000,2020),path=paste0(path_data_directory,"/output/figures/Probability/All"))
get_probability(mydata,shocks,period_range=c(1960,2020),path=paste0(path_data_directory,"/output/figures/Probability/All"))

footnote=c("Grey bars denote the unconditional frequencies of the occurence of shocks. Formally it is the proportion of periods
           with strictly positive tf.idf. Panel (a) shows the heterogeneity of occurence among the entire set of crisis for the 
           entire period. The occurence of event is also high heterogeneous across decades with much lower probability for all crisis
           during the period 1960-1980 and comparable probabilities during 1980-2000 and 2000-2020 altough the ranking is different.
           Among the main evolution we observe the large increase in the probability of financial related crisis that ranks in the 
           top concerns in the recent period with a probability of more than 50 percent.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Probability/All/Probability_shock_footnote.tex"))


ctries=ctry_groups %>% filter(Income_group==" High income")
get_probability(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path=paste0(path_data_directory,"/output/figures/Probability/HighIncome"))

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_probability(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path=paste0(path_data_directory,"/output/figures/Probability/MiddleIncome"))

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_probability(mydata %>% filter(ISO3_Code %in% ctries$iso3c),
              shocks,period_range=c(1960,2020),path=paste0(path_data_directory,"/output/figures/Probability/LowIncome"))

footnote=c("Grey bars denote the unconditional frequencies of the occurence of shocks. Formally it is the proportion of periods
           with strictly positive tf.idf. Important differences across income groups can be observed with economic slowdowns,
           inflations, public debt concerns and financial vulnerabilities as main concerns for high income countries. While Sovereign
           default, natural disasters, inflation and economic slowdowns are the more likely crisis hiting middle and low income countries
    ")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Probability/All/Probability_shock_Incomegroups_footnote.tex"))

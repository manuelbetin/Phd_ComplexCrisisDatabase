#' @title summary figures of probability
#' @description produce the barplot comparison
#' the probability of each crisis
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder Probability

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

# Average data over year:

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)



shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

# Probability of events  ####

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


#ctries=ctry_groups %>% filter(Income_group==" High income")
ctries=ctry_groups %>% filter(Income_group==" High income" & !iso3c %in% c("ATG","BHS","BHR","BRB","BRN","KWT","MAC",
                                                                           "MLT","OMN","PLW","PAN","PRI","QAT","SMR",
                                                                           "SAU","SYC","SGP","KNA","TTO","ARE"))
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

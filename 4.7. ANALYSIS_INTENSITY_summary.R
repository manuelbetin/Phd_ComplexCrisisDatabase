#' @title summary figures of intensity
#' @description produce the barplot comparison
#' the intensity of each crisis
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder Intensity

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions


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
           share of the reports allocated to the given crisis. For instance pressure on public debt, when happening, are 
           allocated by far the largest part of the analysis in all periods. It representes close to 50 percent of the total 
           shocks in 1976-1992 while only around 20 percent in 1950-1976. Amoung high priorities in the past that have seen
           their importance declining overtime we find commodity crisis, migrations, wars or epidemics that were devoted much
           more importance in the early periods.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Intensity/Intensity_shock_footnote.tex"))


# Time series of priority ####

#excludes Sovereign_default
shocks=c("Soft_recession","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

get_first_priority(mydata,shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority"))

ctries=ctry_groups %>% filter(Income_group==" High income")
#ctries=ctry_groups %>% filter(Income_group==" High income")
ctries=ctry_groups %>% filter(Income_group==" High income" & !iso3c %in% c("ATG","BHS","BHR","BRB","BRN","KWT","MAC",
                                                                           "MLT","OMN","PLW","PAN","PRI","QAT","SMR",
                                                                           "SAU","SYC","SGP","KNA","TTO","ARE"))
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority/HighIncome"))

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority/MiddleIncome"))

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_first_priority(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                   path=paste0(path_data_directory,"/output/figures/Priority/LowIncome"))


footnote=c("The figures display the top priority for each year for all countries (panel a) and high income countries (panel b).
           It is computed as the top yearly priority excluding sovereign crisis category that otherwise ranks first every year.
           High income countries exclude small and oil countries.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Priority/priority_ts_footnote.tex"))

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
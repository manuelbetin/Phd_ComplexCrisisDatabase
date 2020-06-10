#' @title Table of architecture of crisis
#' @description script to generate the architecture of the crisis
#' for the country and period selected
#' @author Manuel Betin, Umberto Collodel
#' @return Latex tables

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")


architecture_MEX_1994=get_architecture(mydata,shocks,ctries="MEX",start_year=1993,end_year=1995)

stargazer::stargazer(title="Architecture of the crisis: Mexico 1994"
                     , architecture_MEX_1994
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_MEX_1994.tex")
)

architecture_MEX_1982=get_architecture(mydata,shocks,ctries="MEX",start_year=1978,end_year=1983)


stargazer::stargazer(title="Architecture of the crisis: Mexico 1982"
                     , architecture_MEX_1982
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_MEX_1982.tex")
)



architecture_THA_1997=get_architecture(mydata,shocks,ctries="THA",start_year=1993,end_year=1999)


stargazer::stargazer(title="Architecture of the crisis: Thailand 1997"
                     , architecture_THA_1997
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_THA_1997.tex")
)



architecture_JPN_1990=get_architecture(mydata,shocks,ctries="JPN",start_year=1988,end_year=1992)


stargazer::stargazer(title="Architecture of the crisis: Japan 1990"
                     , architecture_JPN_1990
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_JPN_1990.tex")
)


architecture_USA_2007=get_architecture(mydata,shocks,ctries="USA",start_year=2006,end_year=2010)

stargazer::stargazer(title="Architecture of the crisis: United States 2008"
                     , architecture_USA_2007
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_USA_2007.tex")
)


architecture_USA_1980=get_architecture(mydata,shocks,ctries="USA",start_year=1976,end_year=1982)

stargazer::stargazer(title="Architecture of the crisis: United States 1980"
                     , architecture_USA_1980
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_USA_1980.tex")
)

architecture_USA_1987=get_architecture(mydata,shocks,ctries="USA",start_year=1986,end_year=1988)




architecture_ARG_2001=get_architecture(mydata,shocks,ctries="ARG",start_year=1998,end_year=2003)


stargazer::stargazer(title="Architecture of the crisis: Argentina 2001"
                     , architecture_ARG_2001
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_ARG_2001.tex")
)



architecture_LBN_2016=get_architecture(mydata,shocks,ctries="LBN",start_year=2012,end_year=2018)


stargazer::stargazer(title="Architecture of the crisis: Lebanon 2016"
                     , architecture_LBN_2016
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_LBN_2016.tex")
)



architecture_GRC_2010=get_architecture(mydata,shocks,ctries="GRC",start_year=2008,end_year=2012)


stargazer::stargazer(title="Architecture of the crisis: Greece 2010"
                     , architecture_GRC_2010
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_GRC_2010.tex")
)


architecture_GBR_1990=get_architecture(mydata,shocks,ctries="GBR",start_year=1989,end_year=1994)

stargazer::stargazer(title="Architecture of the crisis: United Kingdom 1990"
                     , architecture_GBR_1990
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture/Architecture_GBR_1990.tex")
)


#ponzi scheeme in Albanie (From Schiller book p94 exuberance irrationnelle)
architecture_ALB_1997=get_architecture(mydata,shocks,ctries="ALB",start_year=1995,end_year=1998)




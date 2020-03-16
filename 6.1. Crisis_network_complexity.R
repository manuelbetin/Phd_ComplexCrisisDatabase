##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
root_path=dirname(current_path)
##install common packages
library("devtools") #make sure you have the library
#install_github("manuelbetin/SetUpProject",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
#install_github("manuelbetin/TextMiningCrisis",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
install_github("manuelbetin/PICindex",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")

packages <- c("dplyr"
              ,"ggplot2"
              ,"plotly"
              ,"pdftools"
              ,"lubridate"
              ,'tictoc'
              ,"rio"
              ,"tidytext"
              ,"stringr"
              ,"stringi"
              ,"tidyr"
              ,"network"
              ,"GGally"
              , "igraph"
              , "TextMiningCrisis"
              , "SetUpProject"
              , "PICindex"
)

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## INSTRUCTIONS ##########

#' script that compute measures of complexity of the network and in particular
#' the degree of the network, the degree distribution and the shortest path
#***************************************************************************************####
output=list()
output[["Session_info"]]=sessionInfo()

# Import data ####

#Complete information combining tf, url metadata and quantititve measures
data("PICdata")
mydata=PICdata
rm(PICdata)
#mydata=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData")


shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',
         'Severe_recession','Sovereign_default',"Currency_crisis_severe","Wars","Social_crisis")


# correlation matrix of shocks ####
mymin=1960
mymax=2016
corr=mydata %>% ungroup() %>% mutate(year=year(period))%>%
  filter(type%in%c("request","consultation","review"))%>%
  filter(year>mymin & year<=mymax)%>%
  dplyr::select(shocks) %>%
  cor() %>% data.frame()

#degree of the network #####

network_degree(mydata,shocks=shocks)

#degree distribution #####

netdistrib_60_90=network_degree_distrib(mydata,
                                        period_range=c(1960,1990),
                                        shocks=shocks)

netdistrib_95_2000=network_degree_distrib(mydata,
                                          period_range=c(1995,2000),
                                          shocks=shocks)

netdistrib_2000_2016=network_degree_distrib(mydata,
                                            period_range=c(2000,2016),
                                            shocks=shocks)

ggplot()+
  geom_point(data=netdistrib_60_90$sumbycorr,aes(x=rownames(netdistrib_60_90$sumbycorr),y=sumlinks,color="1960-1990"))+
  geom_line(data=netdistrib_60_90$sumbycorr,aes(x=rownames(netdistrib_60_90$sumbycorr),y=sumlinks,color="1960-1990",group="1960-1990"))+
  geom_point(data=netdistrib_95_2000$sumbycorr,aes(x=rownames(netdistrib_95_2000$sumbycorr),y=sumlinks,color="1995-2000"))+
  geom_line(data=netdistrib_95_2000$sumbycorr,aes(x=rownames(netdistrib_95_2000$sumbycorr),y=sumlinks,color="1995-2000",group="1995-2000"))+
  geom_point(data=netdistrib_2000_2016$sumbycorr,aes(x=rownames(netdistrib_2000_2016$sumbycorr),y=sumlinks,color="2000-2016"))+
  geom_line(data=netdistrib_2000_2016$sumbycorr,aes(x=rownames(netdistrib_2000_2016$sumbycorr),y=sumlinks,color="2000-2016",group="2000-2016"))+
  #lims(y=c(0,15))+
  labs(x=NULL,y=NULL)+
  theme_minimal()+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/network/degree_distribution.png")

#degree distribution #####

shortdist_banking=network_shortdist(mydata,shocks=shocks,
                                    period_range=c(1990,2016),
                                    root_node = "Expectations",
                                    min_cor=0.4,min_dist=0)

stargazer(shortdist_banking,summary=F)









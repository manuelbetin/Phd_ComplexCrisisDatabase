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
              , "TextMiningCrisis"
              , "SetUpProject"
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
mydata=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData")

# correlation matrix of shocks ####
mymin=1960
mymax=2016
corr=mydata %>% ungroup() %>% mutate(year=year(Period))%>%
  filter(type%in%c("request","consultation","review"))%>%
  filter(year>mymin & year<=mymax)%>%
  dplyr::select(shocks) %>%
  cor() %>% data.frame()

#degree of the network #####

shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',
         'Severe_recession','Sovereign_default',"Currency_crisis_severe","Wars","Social_crisis")


network_degree=function(mydata,
                        Period_range=c(1960,2016),
                        shocks=c("Sovereign_default","Banking_crisis","Financial_crisis"),
                        min_cor=0){
  #'Compute degree of the correlation matrix
  #'
  #'From a database containing the shocks the function compute the degree of each nodes (shocks) and
  #'the average degree of the network
  
  #'@param mydata a tibble with shocks in columns 
  #'@param Period_range a vector containing the first year and last year to consider
  #'@param Shocks a vector with the names of the shocks to consider in the network
  #'@param min_cor the minimum correlation to consider as a valid link between nodes 
  #'
  #'@return a list with $nodes that provide the names of the nodes in the network,
  #'$degree the degree of each node, $avg_degree the average degree of the network
  
  corr=mydata %>% ungroup() %>% mutate(year=year(Period))%>%
    filter(type%in%c("request","consultation","review"))%>%
    filter(year>Period_range[1] & year<=Period_range[2])%>%
    dplyr::select(shocks) %>%
    cor() %>% data.frame()
  
  degree_threshold=function(x){
    ifelse(x>min_cor,x,0)
  }
  
  node_degree=corr %>% mutate_all(degree_threshold) %>% summarize_all(sum)/2
  avg_degree =node_degree %>% rowMeans()
  return(list(nodes=shocks,degree=node_degree,avg_degree=avg_degree))
}


network_degree(mydata,shocks=shocks)

#degree distribution

network_degree_distrib=function(mydata,
                                Period_range=c(1960,2016),
                                shocks=c("Sovereign_default","Banking_crisis","Financial_crisis")
){
  #'Compute degree distribution of the correlation matrix
  #'
  #'From a database containing the shocks the function compute the degree distribution of each nodes (shocks)
  #'
  
  #'@param mydata a tibble with shocks in columns 
  #'@param Period_range a vector containing the first year and last year to consider
  #'@param Shocks a vector with the names of the shocks to consider in the network
  #'
  #'@return a list with $distribution that provide matrix of degree distribution in the network,
  #'$sumbynode the sum of the degree for each node , $sumbycorr the number of degree for each decile 
  #'of correlations
  
  
  corr=mydata %>% ungroup() %>% mutate(year=year(Period))%>%
    filter(type%in%c("request","consultation","review"))%>%
    filter(year>Period_range[1] & year<=Period_range[2])%>%
    dplyr::select(shocks) %>%
    cor() %>% data.frame()
  
  degree_distribution=lapply(seq(0,0.9,0.1),function(x){
    degree_threshold=function(y){
      ifelse(round(y,1)==x,1,0)
    }
    corr %>% mutate_all(degree_threshold) %>% summarize_all(sum)/2
  })
  degree_distribution=do.call(rbind,degree_distribution)
  rownames(degree_distribution)=paste0("Cor",seq(0,0.9,0.1))
  
  rowsumlinks=degree_distribution %>% rowSums()%>% data.frame()%>% rename(sumlinks=".") 
  colsumlinks=degree_distribution %>% colSums() %>% data.frame()%>% rename(sumlinks=".")
  
  return(list(distribution=degree_distribution,sumbynode=colsumlinks,sumbycorr=rowsumlinks))
}


netdistrib_60_90=network_degree_distrib(mydata,
                                        Period_range=c(1960,1990),
                                        shocks=shocks)

netdistrib_95_2000=network_degree_distrib(mydata,
                                          Period_range=c(1995,2000),
                                          shocks=shocks)

netdistrib_2000_2016=network_degree_distrib(mydata,
                                            Period_range=c(2000,2016),
                                            shocks=shocks)

ggplot()+
  geom_point(data=netdistrib_60_90$sumbycorr,aes(x=rownames(netdistrib_60_90$sumbycorr),y=sumlinks,color="1960-1990"))+
  geom_point(data=netdistrib_95_2000$sumbycorr,aes(x=rownames(netdistrib_95_2000$sumbycorr),y=sumlinks,color="1995-2000"))+
  geom_point(data=netdistrib_2000_2016$sumbycorr,aes(x=rownames(netdistrib_2000_2016$sumbycorr),y=sumlinks,color="2000-2016"))+
  #lims(y=c(0,15))+
  labs(x=NULL,y=NULL)+
  theme_minimal()

#shortest path from one node to other
network_shortdist=function(mydata,
                           Period_range=c(1960,2016),
                           shocks=c("Sovereign_default","Banking_crisis","Financial_crisis"),
                           root_node="Sovereign_default",
                           min_cor=0,
                           min_dist=0){
  
  #'Compute the shortest node distance 
  #'
  #'From a database containing the shocks the function use the BFS algorithm to 
  #'find the shortest path between the selected node and the rest of the nodes.
  
  #'@param mydata a tibble with shocks in columns 
  #'@param Period_range a vector containing the first year and last year to consider
  #'@param Shocks a vector with the names of the shocks to consider in the network
  #'@param root_node The name of the reference node
  #'@param min_cor the minimum correlation to consider as a valid link between nodes 
  #'@param max_dist the lowerbound for the distance to display
  #'
  #'@return a dataframe with the shortest distance between the selected node and the rest of the nodes
  
  library(igraph)
  
  #correlation matrix
  corr=mydata %>% ungroup() %>% mutate(year=year(Period))%>%
    filter(type%in%c("request","consultation","review"))%>%
    filter(year>Period_range[1] & year<=Period_range[2])%>%
    dplyr::select(shocks) %>%
    cor()
  #remove links under certain threshold
  corr[corr<=min_cor]=0
  #draw the network
  mygraph=graph_from_adjacency_matrix(corr,weighted=T, mode="undirected", diag=F)
  
  #plot(mygraph)
  #rownames(corr)
  #use bfs algorithm to find the shortest path
  mybfs=igraph::bfs(mygraph,root=root_node,dist=1)
  dist=mybfs$dist %>% data.frame()
  dist$shocks=rownames(dist)
  dist %>% data.frame() %>%
    rename(short_dist=".") %>% 
    filter(short_dist>=min_dist) %>% 
    dplyr::select(shocks,short_dist)
}

shortdist_banking=network_shortdist(mydata,shocks=shocks,
                                    Period_range=c(1990,2016),
                                    root_node = "Expectations",
                                    min_cor=0.2,min_dist=0)

shortdist_banking


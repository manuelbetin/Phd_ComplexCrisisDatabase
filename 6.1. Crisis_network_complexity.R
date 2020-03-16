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
min_cor=0
corr=mydata %>% ungroup() %>% mutate(year=year(period))%>%
  filter(type%in%c("request","consultation","review"))%>%
  filter(year>mymin & year<=mymax)%>%
  dplyr::select(shocks) %>%
  cor()

corr[corr<=min_cor]=0
mygraph=graph_from_adjacency_matrix(corr,weighted=T, mode="undirected", diag=F)

V(mygraph)$size <- 8
V(mygraph)$frame.color <- "grey"
#colors
clp <- cluster_optimal(mygraph)
V(mygraph)$community <- clp$membership
colrs <- adjustcolor( 1:length(clp$membership), alpha=.6)
V(mygraph)$color <- colrs[V(mygraph)$community]
#labels
#V(mygraph)$label <- "" 
E(mygraph)$arrow.mode <- 0
mygraph <- delete_edges(mygraph, E(mygraph)[weight<0.25])

news.path <- shortest_paths(mygraph, 
                            from = V(mygraph)[mygraph=="Banking_crisis"], 
                            to  = V(mygraph)[mygraph=="Commodity_crisis"],
                            output = "both") # both path nodes and edges

plot(mygraph,layout=layout,vertex.color=colrs[V(mygraph)$community])

#layout=layout_with_fr(mygraph)
#layout <- layout_with_fr(mygraph, weights=weights)
#layout <- layout_with_graphopt(mygraph, charge=0.1)
layout <- layout_with_kk(mygraph)

layout = layout_on_sphere(mygraph)
rglplot(mygraph,layout=layout)

#centrality measures

#degree of the network #####

network_degree(mydata,shocks=shocks)

degree(mygraph)/length(shocks)

#degree distribution #####

netdistrib_60_90=network_degree_distrib(mydata,
                                        period_range=c(1960,1990),
                                        shocks=shocks,
                                        round=1)

netdistrib_95_2000=network_degree_distrib(mydata,
                                          period_range=c(1995,2000),
                                          shocks=shocks,
                                          round=1)

netdistrib_2000_2016=network_degree_distrib(mydata,
                                            period_range=c(2000,2016),
                                            shocks=shocks,
                                            round=1)

ggplot()+
  geom_point(data=netdistrib_60_90$sumbycorr,aes(x=rownames(netdistrib_60_90$sumbycorr),y=sumlinks,color="1960-1990"))+
  geom_line(data=netdistrib_60_90$sumbycorr,aes(x=rownames(netdistrib_60_90$sumbycorr),y=sumlinks,color="1960-1990",group="1960-1990"))+
  geom_point(data=netdistrib_95_2000$sumbycorr,aes(x=rownames(netdistrib_95_2000$sumbycorr),y=sumlinks,color="1995-2000"))+
  geom_line(data=netdistrib_95_2000$sumbycorr,aes(x=rownames(netdistrib_95_2000$sumbycorr),y=sumlinks,color="1995-2000",group="1995-2000"))+
  geom_point(data=netdistrib_2000_2016$sumbycorr,aes(x=rownames(netdistrib_2000_2016$sumbycorr),y=sumlinks,color="2000-2016"))+
  geom_line(data=netdistrib_2000_2016$sumbycorr,aes(x=rownames(netdistrib_2000_2016$sumbycorr),y=sumlinks,color="2000-2016",group="2000-2016"))+
  #lims(y=c(0,15))+
  labs(x=NULL,y=NULL)+
  theme_minimal()+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/network/degree_distribution.png")

#degree distribution #####


short_dist=lapply(shocks,function(x){
 dt=network_shortdist(mydata,shocks=shocks,
                    period_range=c(mymin,mymax),
                    root_node = x,
                    min_cor=min_cor,min_dist=0) %>% dplyr::select(-shocks)
  names(dt)=x
dt
})
short_dist=do.call(cbind,short_dist)
rownames(short_dist)=shocks

stargazer(shortdist_banking,summary=F)

# cluster coefficient #####

network_clustercoef(mydata=mydata,
                    period_range=c(mymin,mymax),
                    shocks=shocks,
                    min_cor=min_cor,
                    cluster_type = "local")

#closeness coefficient ####
network_closeness(mydata,period_range=c(mymin,mymax),shocks=shocks,min_cor = min_cor)
network_betweenness(mydata,period_range=c(mymin,mymax),shocks=shocks,min_cor = min_cor)
network_eigencentrality(mydata,period_range=c(mymin,mymax),shocks=shocks,min_cor = min_cor)


# cliques ####
network_cliques(mydata,shocks=shocks,
                period_range=c(1980,1990),
                min_cor = min_cor)



#average nearest neighbor degree 
knn(mygraph)
count_triangles(mygraph,"Trade_crisis")


a=make_ego_graph(mygraph, order = 2, nodes = shocks, mode = c("all",
                                                            "out", "in"), mindist = 1)


names(a)=shocks
a
plot(a[["Financial_crisis"]])
plot(mygraph)










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
install_github("manuelbetin/TextMiningCrisis",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181",build_vignettes = T)
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
              ,"animation"
              ,"visNetwork"
              ,"GGally"
              , "igraph"
              ,"stargazer"
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

#mydata=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")

shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',
         'Severe_recession','Sovereign_default',"Currency_crisis_severe","Wars","Social_crisis")


buckets=list(
#  `1960-1970`=c(1960,1970),
  `1970-1980`=c(1970,1980),
  `1980-1995`=c(1980,1995),
  `1995-2005`=c(1995,2005),
  `2005-2016`=c(2005,2016)
)

# drawn network ####
mymin=1960
mymax=2016
min_cor=0.25
corr=mydata %>% ungroup() %>% mutate(year=year(period))%>%
  filter(type%in%c("request","consultation","review"))%>%
  filter(year>mymin & year<=mymax)%>% na.omit()%>%
  dplyr::select(shocks) %>%
  cor()
mygraph=graph_from_adjacency_matrix(corr,weighted=T, mode="undirected", diag=F)


network_links(mydata,shocks=shocks,
              period_range = c(1980,2016),
              type="conditional",
              lag=1,
              pval_threshold = 0.01) %>%  data.frame()
#a %>% data_frame()

network_visnet(mydata,
               period_range=c(1960,2016),
               shocks=shocks,
               type="conditional",
               min_cor=0.01,
               lag=1,
               pval_threshold=1,
               remove_negative=T,
               mode="directed",
               showarrows=T,
               diag=T,
               edgelabel=T,
               main="Macroeconomic crisis",
               submain=NULL,
               background="white",
               dragView=T,
               dragNodes=T,
               node.shape="dot",
               node.font.size=50,
               node.size=15,
               node.color.highlight.background="red",
               node.color.highligh.border="red",
               edge.font.size=30,
               navigationButtons=F)

#visSave(mynet,file="network.html")

SovDefault_incidence=lapply(buckets,function(x){
  jpeg(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Network/incidence/Incidence_",x[1],"-",x[2],".jpg"))
  network_incidence_graph(mydata,
                          period_range=c(x[1],x[2]),
                          shocks=shocks,
                          target="Sovereign_default",
                          min_cor = 0.2)
  dev.off()
})

network_shortdist_graph(mydata,
                        period_range=c(1960,2016),
                        shocks=shocks,
                        shock_start="Sovereign_default",
                        shock_end="Severe_recession",
                        min_cor = 0.1)

# create gif animation
min_cor=0
saveGIF( {
  network_incidence_graph(mydata,
                          period_range=c(2005,2010),
                          shocks=shocks,
                          target="World_outcomes",
                          min_cor = min_cor)

  network_incidence_graph(mydata,
                          period_range=c(2005,2010),
                          shocks=shocks,
                          target="Banking_crisis",
                          min_cor = min_cor)

  network_incidence_graph(mydata,
                          period_range=c(2005,2010),
                          shocks=shocks,
                          target="Financial_crisis",
                          min_cor = min_cor)

  network_incidence_graph(mydata,
                          period_range=c(2005,2010),
                          shocks=shocks,
                          target="Sovereign_default",
                          min_cor = min_cor)
  },
interval = 2
#, movie.name="../Betin_Collodel/2. Text mining IMF_data/output/figures/Network/incidence_network.gif"
)


#centrality measures

#degree of the network #####

network_degree(mydata,shocks=shocks,min_cor = 0.2)

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
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Network/degree_distribution.png")

#degree distribution #####

min_cor=0.2
short_dist=lapply(shocks,function(x){
 dt=network_shortdist(mydata,shocks=shocks,
                    period_range=c(mymin,mymax),
                    root_node = x,
                    type="conditional",
                    mode="directed",
                    diag=F,
                    min_cor=min_cor,
                    min_dist=0) %>% dplyr::select(-shocks)
  names(dt)=x
dt
})
short_dist=do.call(cbind,short_dist)
rownames(short_dist)=shocks

stargazer(short_dist,summary=F)


#Summary of complexity measures

x=buckets[[1]]
type="conditional"
mode="directed"
diag=F
summary_complexity=lapply(buckets,function(x){
  clustercoef=network_clustercoef(mydata=mydata, period_range=c(x[1],x[2]),shocks=shocks,type=type,mode=mode,diag=diag,
                                  min_cor=min_cor, cluster_type = "local")
  closeness=network_closeness(mydata,period_range=c(x[1],x[2]),shocks=shocks,type=type,mode=mode,diag=diag,
                              min_cor=min_cor)
  betweeness=network_betweenness(mydata,period_range=c(x[1],x[2]),shocks=shocks,type=type,mode=mode,diag=diag,
                                 min_cor=min_cor)
  eigencentrality=network_eigencentrality(mydata,period_range=c(x[1],x[2]),shocks=shocks,type=type,mode=mode,diag=diag,
                                          min_cor=min_cor)
  diameter=network_diameter(mydata,period_range=c(x[1],x[2]),shocks=shocks,type=type,mode=mode,diag=diag,
                            min_cor=min_cor)
  #avgknn=knn(mygraph)
  
  corr=mydata %>% ungroup() %>% mutate(year=year(period))%>%
    filter(type%in%c("request","consultation","review"))%>%
    filter(year>x[1] & year<=x[2])%>%
    dplyr::select(shocks) %>%
    cor() %>% mean
  
  
  eigencentrality=network_eigencentrality(mydata,period_range=c(2000,2016),shocks=shocks,type="conditional",mode="directed",diag=F,
                                          min_cor=min_cor)
  value=c(clustercoef,
          mean(closeness),
          mean(betweeness),
          eigencentrality$centralization,
          diameter,
          corr
          #mean(avgknn$knn)
          )
  value
  #cbind(index,value) %>% t() %>% tibble()
  
})

summary_complexity=do.call(rbind,summary_complexity)
colnames(summary_complexity)=c("Cluster",
                            "Closeness",
                            "betweeness",
                            "eignecentrality",
                            "diameter",
                            "correlation"
                            #"nearest neighbor"
                                  )

stargazer(title="Complexity measures"
          , summary_complexity
          , type="latex"
          , digits=2
          , no.space=T
          , align=T
          , summary=F
          , rownames=T
          , table.placement = "H"
          , column.sep.width="3pt"
          , font.size = "footnotesize"
          , notes.align = "l"
          , notes.append=T
          , out="../Betin_Collodel/2. Text mining IMF_data/output/Complexity/summary_complexity.tex"
          )

# cliques ####

iso=c("AUT","BEL","CHE","DEU","DNK","CYP","CZE","ESP","EST","FRA","GBR","GRC","HRV","IRL","ISL","ITA","NLD",
      "NOR","POL","PRT","SVK","SVN")

mydata$iso3c %>% unique()

cliques=network_cliques(mydata %>% filter(iso3c %in% iso),shocks=shocks,
                period_range=c(2000,2016),
                type="unconditional",
                mode="directed",
                diag=F,
                min_cor=0.1,
                lag=1,
                pval_threshold=0.1,
                remove_negative=T,
                min_cliques=3)

cliques$largest.cliques

network_shortdist_graph(mydata%>% filter(iso3c %in% iso),
                        period_range=c(2005,2016),
                        shocks=shocks,
                        shock_start="Sovereign_default",
                        shock_end="Severe_recession",
                        min_cor = 0.1)


graph=network_links(mydata,shocks,type="conditional")








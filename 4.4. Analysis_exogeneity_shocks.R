
max_no_1=function(x){
  max(ifelse(x!=1,x,NA),na.rm=T)
}
mean_no_1=function(x){
  mean(ifelse(x!=1,x,NA),na.rm=T)
}
min_no_1=function(x){
  min(ifelse(x!=1,x,NA),na.rm=T)
}

corr_matrix=mydata %>% ungroup() %>%
  filter(type%in%c("request","consultation","review"))%>% filter(year>min_date)%>%
  dplyr::select(shocks) %>%
  cor() %>% data.frame()

#average correlation
avg_corr_matrix=corr_matrix %>% summarize_all(mean_no_1)
min_corr_matrix=corr_matrix %>% summarize_all(min_no_1)
max_corr_matrix=corr_matrix %>% summarize_all(max_no_1)

ggplot()+
  geom_bar(stat="identity",aes(x=reorder(x=names(avg_corr_matrix),as.numeric(avg_corr_matrix[1,])),y=as.numeric(avg_corr_matrix[1,])))+
  geom_point(aes(x=reorder(x=names(avg_corr_matrix),as.numeric(avg_corr_matrix[1,])),y=as.numeric(min_corr_matrix[1,])),color="red")+
  geom_point(aes(x=reorder(x=names(avg_corr_matrix),as.numeric(avg_corr_matrix[1,])),y=as.numeric(max_corr_matrix[1,])),color="red")+
   labs(x=NULL,y="Average pairwise correlation")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title=element_blank(),axis.text.x=element_text(angle=90,hjust=1))


#correlation for selected shock
corr_matrix %>% dplyr::select(Currency_crisis)
var="Balance_payment_crisis"
ggplot()+
  geom_bar(stat="identity",aes(x=reorder(x=rownames(corr_matrix),as.numeric(corr_matrix[,var])),y=as.numeric(corr_matrix[,var])))+
  theme_bw()+
  theme(legend.position = "bottom",legend.title=element_blank(),axis.text.x=element_text(angle=90,hjust=1))

stargazer::stargazer(title="Systemic crisis"
                     , summary_cluster
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out="../Betin_Collodel/2. Text mining IMF_data/output/Analysis/Clustering/summary_clusters.tex"
)

network_eco_shocks=function(mydata,shocks,min_year=1960,max_year=2016){
  
  corr_matrix=mydata %>% ungroup() %>%
    filter(type%in%c("request","consultation","review"))%>% filter(year>min_year & year<max_year) %>%
    dplyr::select(shocks) %>%
    cor() %>% data.frame()
  
  #average correlation
  avg_corr_matrix=corr_matrix %>% summarize_all(mean_no_1)
  min_corr_matrix=corr_matrix %>% summarize_all(min_no_1)
  max_corr_matrix=corr_matrix %>% summarize_all(max_no_1)
  
  # network analysis
  net = network(corr_matrix,
                #matrix.type = "bipartite",
                names.eval = "weights", 
                directed=T,
                ignore.eval=F)
  
  ## define nodes caracteristics
  intensity=mydata %>% ungroup() %>%
    filter(type%in%c("request","consultation","review"))%>%
    filter(year>min_year & year<max_year) %>% dplyr::select(shocks) %>%
    na.omit() %>% summarize_all(mean)
  
  intensity_all=mydata %>% ungroup() %>%
    filter(type%in%c("request","consultation","review"))%>%
    dplyr::select(shocks) %>%
    na.omit() %>% summarize_all(mean)
  
  net %v% "intensity"=round(intensity/max(intensity_all),2)


  fun=function(x){
    ifelse(x>0,1,0)
  }
  proba=mydata %>% ungroup() %>%
    filter(type%in%c("request","consultation","review"))%>%
    filter(year>min_year & year<max_year) %>% dplyr::select(shocks) %>%
    na.omit() %>% mutate_all(fun) %>%summarize_all(mean)


  net %v% "proba"=round(percent_rank(data.frame(proba[1,]))*10,0)
  
  net %v% "alphaproba"=ifelse(round(percent_rank(data.frame(proba[1,])),1)<=0.3,0.3,round(percent_rank(data.frame(proba[1,])),1))
  
  typology=typology_categories() %>% filter(variable %in% shocks)
  net %v% "typology"=typology$nature_shock
  
  ## define edge
  
  set.edge.attribute(net, "color", ifelse(net %e% "weights" <0.1,"darkgreen",
                                          ifelse(net %e% "weights" > 0.1 & net %e% "weights" < 0.2,"darkblue",
                                                 ifelse(net %e% "weights" > 0.2 & net %e% "weights" < 0.5,"orange",
                                                        ifelse(net %e% "weights" > 0.5,"darkred",0)))))
  
  set.edge.attribute(net, "lty", ifelse(net %e% "weights" <0.1,3,
                                        ifelse(net %e% "weights" > 0.1 & net %e% "weights" < 0.2,3,
                                               ifelse(net %e% "weights" > 0.2 & net %e% "weights" < 0.5,1,
                                                      ifelse(net %e% "weights" > 0.5,1,0)))))
  
  set.edge.attribute(net, "alpha", ifelse(net %e% "weights" <0.1,0.2,
                                          ifelse(net %e% "weights" > 0.1 & net %e% "weights" < 0.5,0.5,
                                                 ifelse(net %e% "weights" > 0.5,1,0))))
  
  set.edge.attribute(net, "abc", ifelse(net %e% "weights">=0.1,net %e% "weights",0.0001)*5)
  
  set.seed(1)
  ggnet2(net,
         mode="target",layout.par = list(niter = 100,circ.rad=2),
          color="darkred",#"typology",
          node.size ="intensity",
          edge.size = "abc",
          #node.alpha = "alphaproba",
          #edge.label = "weights",
          edge.label.alpha = 1,
          edge.label.size = "proba",
          edge.lty = "lty",
          #legend.position = "none",
          alpha=1,
          #label.size = "size_node",
          label.alpha = "alphaproba",
          #label.color="Color",
          edge.color = "color",
          edge.alpha = 0.3,
          #edge.label.color = "black",
          size.zero=T,
          label=T,
          edge.label.fill = NA
          )+
          theme_dark()+
          labs(title=paste0("Mesuring systemic crisis: ",min_year,"-",max_year),
               caption="Edge color, type and size measure the correlation between crisis.
                Red links denote correlations higher than 0.5, orange between 0.2 and 0.5,
                dotted lines correlations between 0.1 and 0.2.
                The size of node measure the average intensity of each crisis.
                The opacity of the label denotes the probability of occurence")+
          theme(panel.background = element_rect(color = "grey15"))

}


buckets=list(
 # bucket1=c(1960,1973),
  bucket1=c(1960,1980),
  bucket2=c(1980,1993),
  bucket3=c(1993,2000),
  bucket4=c(2000,2003),
  bucket5=c(2003,2016)
)

mygraphs=lapply(buckets,function(x){
network_eco_shocks(mydata,shocks,min_year=x[1],max_year = x[2])  
})
names(mygraphs)=names(buckets)
mygraphs$bucket5







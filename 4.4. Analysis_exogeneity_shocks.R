
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
  filter(type%in%c("request","consultation","review"))%>%
  dplyr::select(shocks) %>%
  cor() %>% data.frame()
#corr_matrix$var=names(corr_matrix)

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

library(network)
library(GGally)

net = network(corr_matrix,
              #matrix.type = "bipartite",
              names.eval = "weights", 
              
              directed=T,
              ignore.eval=F)

set.edge.attribute(net, "color", ifelse(net %e% "weights" <0.1,"white",
                                        ifelse(net %e% "weights" > 0.1 & net %e% "weights" < 0.2,"darkgreen",
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



ggnet2(net,
       color="darkred",
       #palette=c("actor"="darkgreen","event"="darkred"),
       #node.size ="corr",
       edge.size = "abc",
       node.alpha = 1,
       #edge.label = "weights",
       edge.label.alpha = 1,
       edge.label.size = 3,
       edge.lty = "lty",
       legend.position = "none",
       alpha=1,
       #label.size = "size_node",
       label.alpha = 1,
       #label.color="Color",
       edge.color = "color",
       edge.alpha = 0.3,
       #edge.label.color = "black",
       size.zero=T,
       label=T,
       edge.label.fill = NA)+
labs(title="Correlation matrix",
       caption="Note: blabla")+
  theme(panel.background = element_rect(color = "grey15"))#+

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


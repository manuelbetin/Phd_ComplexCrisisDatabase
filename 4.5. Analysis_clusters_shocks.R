

cluster_crisis=function(mydata,unit_cluster="iso3c",list_vars,Nclusters=6,N_Name_categories=3){
  
  dt=mydata %>% ungroup() %>%  dplyr::select(unit_cluster) %>% unique()
  
  dt_cluster=mydata %>%
    group_by(get(unit_cluster)) %>% 
    dplyr::select(list_vars) %>% na.omit() %>%
    summarize_all(mean) %>% ungroup() #%>% dplyr::select(-unit_cluster)
  dt_cluster=dt_cluster[,-1]
  dt_cluster=scale(dt_cluster)
  
  set.seed(1)
  dt_kmeans=kmeans(dt_cluster,centers=Nclusters, iter.max=500, nstart=1)
  dt_cluster_avg=aggregate(dt_cluster,by=list(dt_kmeans$cluster),FUN=mean)
  
  dt2 <- data.frame(dt_cluster, dt_kmeans$cluster)
  dt_cluster<-cbind(dt,dt2)
  
  # type_crisis_group=dt_cluster %>% group_by(dt_kmeans.cluster) %>% summarize_if(is.numeric,mean) %>%
  #   gather(key="group",value="values",-dt_kmeans.cluster) %>% arrange(dt_kmeans.cluster,-values) %>% group_by(dt_kmeans.cluster) %>%
  #   top_frac(top_perc) %>% summarize(typology=str_replace_all(paste0(group,collapse="/ "),"_"," "))
  # 
  type_crisis_group=dt_cluster %>% group_by(dt_kmeans.cluster) %>% summarize_if(is.numeric,mean) %>%
    gather(key="group",value="values",-dt_kmeans.cluster) %>% arrange(dt_kmeans.cluster,-values) %>% group_by(dt_kmeans.cluster)
  
  #compute the average correlation between all shocks
  avg_corr_matrix=mydata  %>% ungroup() %>% dplyr::select(shocks) %>% na.omit() %>% cor() %>% data.frame() %>% summarize_all(mean)
  avg_corr_matrix=data.frame(t(avg_corr_matrix))
  avg_corr_matrix$group=rownames(avg_corr_matrix)
  names(avg_corr_matrix)[1]="corr.coef"
  
  #compute the exogeneity index
  type_crisis_group=type_crisis_group %>% left_join(avg_corr_matrix,by=c("group")) %>% mutate(systemicity_index=sum(corr.coef*values,na.rm=T))%>% 
 summarize(typology=str_replace_all(paste0(group,collapse="/ "),"_"," "),
           systemicity_index=mean(systemicity_index,na.rm=T) %>% round(.,2)) %>% rowwise() %>%
    mutate(typology=paste0(unlist(str_split(typology,"/"))[1:N_Name_categories],collapse="/"))
  
  ctry_clusters=dt_cluster %>% group_by(dt_kmeans.cluster) %>%
    summarize(ctries=paste(unique(unit_cluster),collapse = ', '),
              n=n()) %>% 
    left_join(type_crisis_group,by="dt_kmeans.cluster") %>%
    rename(group=dt_kmeans.cluster) %>% ungroup() %>% mutate(prop=round(100*n/sum(n),2)) %>%
    dplyr::select(typology,n,prop,systemicity_index) %>% arrange(-prop)
  
  return( list(dt=dt_cluster,arguments_cluster=list_vars,ctry_cluster_summary=ctry_clusters))
}

cluster=list()
dt=mydata %>% filter(type %in% c("consultation","request","review") & year<1980)
cluster[["bef1980"]]=cluster_crisis(dt,
                                        unit_cluster="file",
                                        list_vars = shocks,
                                        Nclusters=3,
                                        N_Name_categories = 3)
cluster[["bef1980"]]$ctry_cluster_summary=cluster[["bef1980"]]$ctry_cluster_summary %>% mutate(Period="Before 1980") %>% dplyr::select(Period,everything())

dt=mydata %>% filter(type %in% c("consultation","request","review") & (year<2000 & year>=1980))
cluster[["1980_2000"]]=cluster_crisis(dt,
                                    unit_cluster="file",
                                    list_vars = shocks,
                                    Nclusters=3,
                                    N_Name_categories = 3)
cluster[["1980_2000"]]$ctry_cluster_summary=cluster[["1980_2000"]]$ctry_cluster_summary %>% mutate(Period="1980-2000") %>% dplyr::select(Period,everything())

dt=mydata %>% filter(type %in% c("consultation","request","review") & (year>=2000))
cluster[["aft2000"]]=cluster_crisis(dt,
                                    unit_cluster="file",
                                    list_vars = shocks,
                                    Nclusters=3,
                                    N_Name_categories = 3)
cluster[["aft2000"]]$ctry_cluster_summary=cluster[["aft2000"]]$ctry_cluster_summary %>% mutate(Period="2000-2016") %>% dplyr::select(Period,everything())

output[["clusters"]]=cluster


summary_cluster=rbind(cluster[["bef1980"]]$ctry_cluster_summary,
                      cluster[["1980_2000"]]$ctry_cluster_summary,
                      cluster[["aft2000"]]$ctry_cluster_summary)

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
  
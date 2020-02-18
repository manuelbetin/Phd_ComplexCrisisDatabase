
cluster_crisis=function(mydata,unit_cluster="iso3c",list_vars,Nclusters=6,top_perc=1){
  
  dt=mydata %>% ungroup() %>%  dplyr::select(unit_cluster) %>% unique()
  
  dt_cluster=mydata %>% 
    group_by(get(unit_cluster)) %>% 
    dplyr::select(list_vars) %>% 
    summarize_all(mean) %>% ungroup() #%>% dplyr::select(-unit_cluster)
  dt_cluster=dt_cluster[,-1]
  dt_cluster=scale(dt_cluster)
  
  set.seed(1)
  dt_kmeans=kmeans(dt_cluster,centers=Nclusters, iter.max=500, nstart=1)
  dt_cluster_avg=aggregate(dt_cluster,by=list(dt_kmeans$cluster),FUN=mean)
  
  dt2 <- data.frame(dt_cluster, dt_kmeans$cluster)
  dt_cluster<-cbind(dt,dt2)
  
  type_crisis_group=dt_cluster %>% group_by(dt_kmeans.cluster) %>% summarize_if(is.numeric,mean) %>%
    gather(key="group",value="values",-dt_kmeans.cluster) %>% arrange(dt_kmeans.cluster,-values) %>% group_by(dt_kmeans.cluster) %>%
    top_frac(top_perc) %>% summarize(typology=str_replace_all(paste0(group,collapse="/ "),"_"," "))
  
  ctry_clusters=dt_cluster %>% group_by(dt_kmeans.cluster) %>%
    summarize(ctries=paste(unique(unit_cluster),collapse = ', '),
              n=n()) %>% 
    left_join(type_crisis_group,by="dt_kmeans.cluster") %>%
    rename(group=dt_kmeans.cluster) %>% dplyr::select(typology,n,ctries)
  
  return( list(dt=dt_cluster,arguments_cluster=list_vars,ctry_cluster_summary=ctry_clusters))
}


myvars=str_remove(names(mydata)[str_detect(names(mydata),"_norm")],"_norm")

mydata_avg=mydata %>% group_by(iso3c,startdate) %>% 
  mutate_at(vars(myvars),funs(program_sd=sd(.,na.rm=T))) %>% mutate(file=paste0(iso3c,"_",startdate)) %>% dplyr::select(file,everything())


cluster=list()

cluster[["request"]]=cluster_crisis(mydata %>% filter(type=="request"),
                                    unit_cluster="file",
                                    list_vars = shocks,
                                    Nclusters=5,
                                    top_perc = 1)

cluster[["consultation"]]=cluster_crisis(mydata %>% filter(type %in% c("request","review","consultation")),
                                        unit_cluster="file",
                                        list_vars = paste0(shocks,"_program_mean"),
                                        Nclusters=2,
                                        top_perc = 0.2)

cluster[["review"]]=cluster_crisis(mydata_avg %>% filter(type=="review"),
                                   unit_cluster="file",
                                   list_vars = shocks,
                                   Nclusters=3,
                                   top_perc = 1)
# 
# cluster[["first_doc"]]=cluster_crisis(mydata_avg %>% filter(Review_number_new=="1 Request"),
#                                       unit_cluster="file",
#                                       list_vars = shocks,
#                                       Nclusters=3,
#                                       top_perc = 1)
# 
# cluster[["midterm"]]=cluster_crisis(mydata_avg %>% filter(Review_number_new=="2 midterm Review"),
#                                     unit_cluster="file",
#                                     list_vars = shocks,
#                                     Nclusters=3,
#                                     top_perc = 1)
# 
# cluster[["lastdoc"]]=cluster_crisis(mydata_avg %>% filter(Review_number_new=="3 Last Review"),
#                                     unit_cluster="file",
#                                     list_vars = shocks,
#                                     Nclusters=3,
#                                     top_perc = 1)

output[["clusters"]]=cluster


a=cluster[["consultation"]]$ctry_cluster_summary

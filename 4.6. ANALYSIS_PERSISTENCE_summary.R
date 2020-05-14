#' @title summary figures of persistence
#' @description produce the barplot comparison
#' the persistence of each crisis
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder duration



path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

# Average data over year:

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)

library(forcats)

# Duration of events ####

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

get_duration=function(mydata,shocks){
  #' @title get the number of years of each episode of crisis
  #' @description compute the number of years of each episode
  #' of crisis
  #' @param mydata a database with the tfidf
  #' @param shocks the names of the shocks to consider
  #' @author Manuel Betin
  #' @return a dataset with summary statistics on the
  #' duration of episodes
  #' 
  #' 
  ctries=mydata$ISO3_Code %>% unique()
  all=lapply(ctries,function(ctry){
    # var="Sovereign_default"
    dt=lapply(shocks,function(var){
      dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>% 
        filter(ISO3_Code==ctry & year>=1945) %>% ungroup() %>%
        group_by(year)%>%
        summarise_at(vars(shocks),cond_mean) %>%
        mutate_at(vars(shocks),get_prob)%>% dplyr::select(year,var) %>%
        mutate(var:=ifelse(is.na(get(var)),0,get(var))) %>%
        arrange(year) 
      dt=dt %>% na.omit()
      epi=0
      dt$episode=0
      count=0
      for(i in 1:(dim(dt)[1]-1)){
        if(i+count<dim(dt)[1]-1){
          i=i+count
          if(dt[[i,var]]>0){
            count=1
            epi=epi+1
            dt[i,"episode"]=epi
            dt[i,var]=count
            if(i+count<dim(dt)[1]-1){
              while(dt[[ifelse(i+count>=dim(dt)[1],dim(dt)[1],i+count),var]]==1 & count<20){
                count=count+1
                dt[i,var]=count
                dt[i,"episode"]=epi
              }
            }
          }
        }
      }
      dt=dt %>% filter(episode>0)
      dt=dt[,1:2]
      summary(dt[,var],na.rm=T) %>% data.frame()
    })
    names(dt)=shocks
    dt=do.call(rbind,dt) %>% dplyr::select(shocks=Var2,stat=Freq) %>%
      mutate(statistic=substr(stat,1,7),
             stat=as.numeric(substr(stat,9,13)))
    dt$ISO3_Code=ctry
    print(ctry)
    dt
  })
  all=do.call(rbind,all)
  all=data.frame(all)
  return(all)
}

get_duration_fig=function(mydata,shocks,lowerbound=0,path=NULL){
  #' @title barplot of duration of crisis
  #' @description ggplot figure showing the average duration 
  #' of episodes
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  avg_duration=get_duration(mydata,shocks)
  
  avg_dur=avg_duration %>%
    group_by(shocks,statistic) %>%
    summarize(mean=mean(stat,na.rm=T)) %>%
    spread(key=statistic,value=mean) %>%
    ungroup()
  colnames(avg_dur)=c("shocks","p25","p75","max","mean","median","min")
  avg_dur=avg_dur%>% ungroup() %>% mutate(shocks = fct_reorder(shocks,mean)) 
  
  myfig=ggplot(avg_dur)+
    geom_errorbar(aes(x=shocks,ymin = min, ymax = max),color="grey")+
    #geom_bar(stat="identity",aes(x=shocks,y=mean),fill="darkgrey",col = "black",alpha=0.6)+
    geom_point(aes(x=shocks,y=mean),fill = "red",alpha=0.6,shape=21)+
    geom_text(aes(x=shocks,y=max,label=round(max,1)),color = "grey",alpha=1,vjust=-1)+
    geom_text(aes(x=shocks,y=min,label=round(min,1)),color = "grey",alpha=1,vjust=1)+
    geom_text(aes(x=shocks,y=mean,label=round(mean,1)),color = "black",alpha=1,vjust=-1)+
    theme_bw()+
    labs(y="N. years",
         x=NULL,
         title=NULL)+
    lims(y=c(-1,max(avg_dur$max)+5))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =11,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="bottom")
  
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("duration_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}  


duration_table=get_duration(mydata,shocks)

avg_dur=duration_table %>%
  group_by(shocks,statistic) %>%
  summarize(mean=mean(stat,na.rm=T)) %>%
  spread(key=statistic,value=mean) %>%
  ungroup()
colnames(avg_dur)=c("shocks","p25","p75","max","mean","median","min")
avg_dur=avg_dur%>% ungroup() %>% mutate(shocks = fct_reorder(shocks,mean)) 
avg_dur=avg_dur %>% dplyr::select(shocks,mean,min,p25,median,p75,max) %>% mutate(shocks=str_remove_all(shocks," "))
avg_dur=avg_dur %>% mutate(mean=round(mean,2),
                           min=round(min,2),
                           p25=round(p25,2),
                           median=round(median,2),
                           p75=round(p75,2),
                           max=round(max,2)) %>% arrange(-mean)

stargazer::stargazer(title="Persistence: summary table"
                     , avg_dur
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=T
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/duration/duration_summary.tex")
)

get_duration_fig(mydata,shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration"))

ctries=ctry_groups %>% filter(Income_group==" High income")
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration/HighIncome"))

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration/MiddleIncome"))

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration/LowIncome"))

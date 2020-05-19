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

get_duration=function(mydata,shocks,lowerbound=0){
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
  #' 
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  ctry="ATG"
  var="Financial_crisis"
  ctries=mydata$ISO3_Code %>% unique()
  all=lapply(ctries,function(ctry){
    print(ctry)
    dt=lapply(shocks,function(var){
      #print(var)
      dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>% 
        filter(ISO3_Code==ctry & year>=1945) %>% ungroup() %>%
        group_by(year)%>%
        summarise_at(vars(shocks),cond_mean) %>%
        mutate_at(vars(shocks),get_prob)%>% dplyr::select(year,var) %>%
        mutate(var:=ifelse(is.na(get(var)),0,get(var))) %>%
        arrange(year) 
      epi=0
      dt$episode=0
      count=0
      for(i in 2:(dim(dt)[1])){
        if(i+count<=dim(dt)[1]){
          i=i+count
          if(dt[[i,"var"]]>0 & dt[[i-1,"var"]]==0){
            count=1
            epi=ifelse(count==1,epi+1,epi)
            dt[i,"episode"]=epi
            dt[i,var]=count
            if(i+count<=dim(dt)[1]){
              while(dt[[ifelse(i+count>=dim(dt)[1],dim(dt)[1],i+count),"var"]]==1 & i+count<=dim(dt)[1]){
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
      colnames(dt)[2]="duration"
      if(dim(dt)[1]!=0){
        dt$ISO3_Code=ctry
        dt$shocks=var
        dt  
      }else{
        dt=NA
      }
      #summary(dt[,var],na.rm=T) %>% data.frame()
    })
    names(dt)=shocks
    dt=do.call(rbind,dt) 
    if(dim(dt)[2]==1){
     NA
    }else{
      dt
    }
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
    group_by(shocks) %>%
    summarize(mean=mean(duration,na.rm=T),
              min=min(duration,na.rm=T),
              p25=quantile(duration,0.25,na.rm=T),
              median=quantile(duration,0.5,na.rm=T),
              p75=quantile(duration,0.75,na.rm=T),
              p95=quantile(duration,0.95,na.rm=T),
              max=max(duration,na.rm=T)) %>%
    arrange(-mean) %>% filter(!is.na(shocks)) %>%
    mutate(shocks=ifelse(shocks=="Balance_payment_crisis","B.o.P.",shocks),
           shocks=ifelse(shocks=="World_outcomes","World",shocks),
           shocks=ifelse(shocks=="Sovereign_default","Sovereign",shocks),
           shocks=ifelse(shocks=="Natural_disaster","Nat. disaster",shocks),
           shocks=ifelse(shocks=="Currency_crisis_severe","Currency",shocks),
           shocks=ifelse(shocks=="Soft_recession","Eco. slowdown",shocks),
           shocks=ifelse(shocks=="Severe_recession","Eco. recession",shocks),
           shocks=gsub("_","",shocks),
           shocks=gsub("crisis","",shocks),
           shocks = fct_reorder(shocks,mean))
  
  myfig=ggplot(avg_dur)+
    geom_errorbar(aes(x=shocks,ymin = p25, ymax = p95),color="grey")+
    #geom_bar(stat="identity",aes(x=shocks,y=mean),fill="darkgrey",col = "black",alpha=0.6)+
    geom_point(aes(x=shocks,y=mean),fill = "red",alpha=0.6,shape=21)+
    geom_text(aes(x=shocks,y=p95,label=round(p95,1)),color = "grey",alpha=1,vjust=-2,size=4)+
    geom_text(aes(x=shocks,y=p25,label=round(p25,1)),color = "grey",alpha=1,vjust=2,size=4)+
    geom_text(aes(x=shocks,y=mean,label=round(mean,1)),color = "black",alpha=1,vjust=-1,size=4)+
    theme_bw()+
    labs(y="years",
         x=NULL,
         title=NULL)+
    lims(y=c(-3,max(avg_dur$p95)+5))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15,angle=90, hjust =1,vjust =0.5),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
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
  group_by(shocks) %>%
  summarize(mean=mean(duration,na.rm=T) %>% round(.,2),
            min=min(duration,na.rm=T) %>% round(.,2),
            p25=quantile(duration,0.25,na.rm=T) %>% round(.,2),
            median=quantile(duration,0.5,na.rm=T) %>% round(.,2),
            p75=quantile(duration,0.75,na.rm=T) %>% round(.,2),
            p95=quantile(duration,0.95,na.rm=T) %>% round(.,2),
            max=max(duration,na.rm=T) %>% round(.,2)) %>%
    arrange(-mean) %>% filter(!is.na(shocks)) %>%
  mutate(shocks=ifelse(shocks=="Balance_payment_crisis","B.o.P.",shocks),
         shocks=ifelse(shocks=="World_outcomes","World",shocks),
         shocks=ifelse(shocks=="Sovereign_default","Sovereign",shocks),
         shocks=ifelse(shocks=="Natural_disaster","Nat. disaster",shocks),
         shocks=ifelse(shocks=="Currency_crisis_severe","Currency",shocks),
         shocks=ifelse(shocks=="Soft_recession","Eco. slowdown",shocks),
         shocks=ifelse(shocks=="Severe_recession","Eco. recession",shocks),
         shocks=gsub("_","",shocks),
         shocks=gsub("crisis","",shocks)
         ) %>% dplyr::select(-min)
  
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

# ctries=c("USA")
# get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries),shocks=shocks)



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

footnote=c("Red dot denote the duration in years of each crisis measure as the number of consecutive years with 
stricly positive tf.idf. horizontal lines denote the 25 percentile and 95 percentile of the duration of each crisis.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Duration/Duration_shock_footnote.tex"))




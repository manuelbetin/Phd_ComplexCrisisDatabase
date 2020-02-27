
systemic_ts=function(mydata,shocks=c("Banking_crisis","Financial_crisis","Expectations"),
                     alpha=1,beta=1,gamma=1){
  #' Compute crisis index.
  #'
  #' Combine probability of shocks, intensity and complexity of relations to construct
  #' .a mesure of severity of crisis
  #'
  #' @param shocks provide a vector with the name of the shocks that you want to consider
  #' @param alpha weight given to the probability of shocks 
  #' @param beta weight given to the intensity of shocks
  #' @param gamma weight given to the complexity
  #' 
  #' @return a data frame containing the crisis index and each component
  #' or not.
  #'
  #' @examples
  #'
  #'
  #' @export
  
  
  mymin=seq(1961,2015,1)
  mymax=seq(1962,2016,1)
  #systemic_index=array(NA,dim=c(length(mymin),4,length(shocks)),dimnames=list(mymin,c("crisis","corr","PD","LGD"),shocks))
  systemic_index=array(NA,dim=c(length(mymin),4),dimnames=list(mymin,c("crisis","corr","PD","LGD")))
  
  for(i in 1:length(mymin)){
    
    PD=function(x){
      mean(ifelse(x>0,1,0),na.rm=T)
    }
    LGD=function(x){
      mean(x,na.rm=T)
    }
    correct_cor=function(x){
      replace_na(x,0)
    }
    
    mean_no_1=function(x){
      replace_na(ifelse(is.na(mean(ifelse(x!=1,x,NA),na.rm=T)),0,mean(ifelse(x!=1,x,NA),na.rm=T)),0)
    }
    
    corr=mydata %>% ungroup() %>% mutate(year=year(Period))%>%
      filter(type%in%c("request","consultation","review"))%>%
      filter(year>mymin[i] & year<=mymax[i])%>%
      dplyr::select(shocks) %>%
      cor() %>% data.frame()%>%
      summarize_all(mean_no_1) 
    
    PD=mydata %>%mutate(year=year(Period))%>% filter(year>mymin[i] & year<=mymax[i])%>% ungroup()  %>% dplyr::select(shocks) %>% summarize_all(PD)
    
    LGD=mydata %>%mutate(year=year(Period))%>% filter(year>mymin[i] & year<=mymax[i])%>% ungroup()  %>% dplyr::select(shocks) %>% summarize_all(LGD)
    
    systemic_index[i,"crisis"]=(PD^alpha*(LGD)^beta*exp(corr)^gamma) %>% sum()
    systemic_index[i,"corr"]=exp(corr) %>% sum()
    systemic_index[i,"PD"]=PD %>% sum()
    systemic_index[i,"LGD"]=LGD %>% sum()
  }
  return(systemic_index)
    
}

plot_systemic_ts=function(mydata,shocks,alpha=1,beta=1,gamma=1){
  dt=mydata #%>% filter(iso3c %in% subsample_countries$ISO3_Code)
  dt1=systemic_ts(dt,shocks=shocks,alpha=alpha,beta=beta,gamma=gamma)
  dt2=systemic_ts(dt,shocks=shocks,alpha=1,beta=0,gamma=0)
  dt3=systemic_ts(dt,shocks=shocks,alpha=0,beta=1,gamma=0)
  dt4=systemic_ts(dt,shocks=shocks,alpha=0.1,beta=0.1,gamma=1)
  
  ggplot()+
    geom_line(aes(x=as.numeric(rownames(dt1)),y=(dt1[,"crisis"]-mean(dt1[,"crisis"],na.rm=T))/sd(dt1[,"crisis"],na.rm=T),color="1.Composite"),size=1)+
    geom_point(aes(x=as.numeric(rownames(dt1)),y=(dt1[,"crisis"]-mean(dt1[,"crisis"],na.rm=T))/sd(dt1[,"crisis"],na.rm=T),color="1.Composite"))+
    geom_line(aes(x=as.numeric(rownames(dt1)),y=(dt2[,"crisis"]-mean(dt2[,"crisis"],na.rm=T))/sd(dt2[,"crisis"],na.rm=T),color="2.PD"),alpha=0.5)+
    #  geom_point(aes(x=as.numeric(rownames(dt1)),y=dt2[,"crisis"]/mean(dt2[,"crisis"],na.rm=T),color="2.PD"),alpha=0.5)+
    geom_line(aes(x=as.numeric(rownames(dt1)),y=(dt3[,"crisis"]-mean(dt3[,"crisis"],na.rm=T))/sd(dt3[,"crisis"],na.rm=T),color="3.LGD"),alpha=0.5)+
    #   geom_point(aes(x=as.numeric(rownames(dt1)),y=dt3[,"crisis"]/mean(dt3[,"crisis"],na.rm=T),color="3.LGD"),alpha=0.5)+
    geom_line(aes(x=as.numeric(rownames(dt1)),y=(dt4[,"crisis"]-mean(dt4[,"crisis"],na.rm=T))/sd(dt4[,"crisis"],na.rm=T),color="4.Complex"),alpha=0.5)+
    #  geom_point(aes(x=as.numeric(rownames(dt1)),y=dt4[,"crisis"]/mean(dt4[,"crisis"],na.rm=T),color="4.Complex"),alpha=0.5)+
    scale_x_discrete(limits=c(1960:2016),breaks=seq(1960,2016,2))+
    theme_minimal()+
    labs(x=NULL,y=NULL)+
    theme(axis.text.x = element_text(angle=90))
  
}

shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',
         'Severe_recession','Sovereign_default',"Currency_crisis_severe","Wars","Social_crisis")

Composite_crisis=plot_systemic_ts(mydata,shocks=shocks)
Composite_crisis+labs(title="Composite")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_crisis.png")

economic_crisis=plot_systemic_ts(mydata,shocks=c("World_outcomes","Severe_recession","Soft_recession"))
economic_crisis+labs(title="Economic")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Economic_crisis.png")

Financial_crisis=plot_systemic_ts(mydata,shocks=c("Financial_crisis","Banking_crisis","Contagion","Expectations"))
Financial_crisis+labs(title="Financial")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Financial_crisis.png")

Fiscal_crisis=plot_systemic_ts(mydata,shocks=c("Fiscal_outcomes","Fiscal_consolidation","Sovereign_default"))
Fiscal_crisis+labs(title="Fiscal")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Fiscal_crisis.png")

Exogeneous=plot_systemic_ts(mydata,shocks=c("Trade_crisis","Natural_disaster","Commodity_crisis"))
Exogeneous+labs(title="Exogeneous")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Exogeneous_crisis.png")

Currency=plot_systemic_ts(mydata,shocks=c("Currency_crisis","Inflation_crisis","Balance_payment_crisis","Contagion",
                                          'Expectations',"Reduction_reserves"))

Currency+labs(title="Exogeneous")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Currency_crisis.png")




mean_no_1=function(x){
  replace_na(ifelse(is.na(mean(ifelse(x!=1,x,NA),na.rm=T)),0,mean(ifelse(x!=1,x,NA),na.rm=T)),0)
}

corr=mydata %>% ungroup() %>% mutate(year=year(Period))%>%
  filter(type%in%c("request","consultation","review"))%>%
 # filter(year>mymin[i] & year<=mymax[i])%>%
  dplyr::select(shocks) %>%
  cor() %>% data.frame()#%>%
  #summarize_all(mean_no_1) 


degree_threshold=function(x){
  ifelse(x>0.1,x,0)
}
degree=corr %>% mutate_all(degree_threshold) %>% summarize_all(sum)

degree_threshold=function(x){
  ifelse(x>0.2,x,0)
}
top_degree=corr %>% mutate_all(degree_threshold) %>% summarize_all(sum)

web=corr %>% mutate_all(degree_threshold)
net=network(web)
ggnet2(net)

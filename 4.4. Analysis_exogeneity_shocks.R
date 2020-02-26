
max_no_1=function(x){
  max(ifelse(x!=1,x,NA),na.rm=T)
}
mean_no_1=function(x){
  ifelse(is.na(mean(ifelse(x!=1,x,NA),na.rm=T)),0,mean(ifelse(x!=1,x,NA),na.rm=T))
}
min_no_1=function(x){
  min(ifelse(x!=1,x,NA),na.rm=T)
}

min_date=1960
corr_matrix=mydata %>% ungroup() %>%
  filter(type%in%c("request","consultation","review"))%>% filter(year>min_date)%>%
  dplyr::select(shocks) %>%
  cor() %>% data.frame()

#average correlation
avg_corr_matrix=corr_matrix %>% summarize_all(mean_no_1)
min_corr_matrix=corr_matrix %>% summarize_all(min_no_1)
max_corr_matrix=corr_matrix %>% summarize_all(max_no_1)

#correlation for selected shock
corr_matrix %>% dplyr::select(Currency_crisis_severe)
var="Balance_payment_crisis"
ggplot()+
  geom_bar(stat="identity",aes(x=reorder(x=rownames(corr_matrix),as.numeric(corr_matrix[,var])),y=as.numeric(corr_matrix[,var])))+
  theme_bw()+
  theme(legend.position = "bottom",legend.title=element_blank(),axis.text.x=element_text(angle=90,hjust=1))



# shocks=typology_categories() %>% filter(!nature_shock %in% c("Economic_ajustement",NA) &
#                                           !type %in% c("debt_structure")) %>% unique()
# 
# shocks=typology_categories() %>% filter(type %in% c("adjustment_program","debt_outcome")) %>% unique()
# 
# shocks=shocks$variable
# 
# shocks=c("Deregulation","Reform_agenda",
#          "Labor_market_reforms",
#          "Tax_reforms",
#          "Fiscal_consolidation","Fiscal_consolidation")

systemic_ts=function(shocks=c("Banking_crisis","Financial_crisis","Expectations"),alpha=1,beta=1,gamma=1){

  mymin=seq(1960,2014,2)
  mymax=seq(1962,2016,2)
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
    corr=mydata %>% ungroup() %>%
      filter(type%in%c("request","consultation","review"))%>%
      filter(year>mymin[i] & year<=mymax[i])%>%
      dplyr::select(shocks) %>%
      cor() %>% data.frame()%>%
      summarize_all(mean_no_1) 
    
    PD=mydata %>% filter(year>mymin[i] & year<=mymax[i])%>% ungroup()  %>% dplyr::select(shocks) %>% summarize_all(PD)
    
    LGD=mydata %>% filter(year>mymin[i] & year<=mymax[i])%>% ungroup()  %>% dplyr::select(shocks) %>% summarize_all(LGD)
    
    systemic_index[i,"crisis"]=(PD^alpha*LGD^beta*exp(corr)^gamma) %>% sum()
    systemic_index[i,"corr"]=corr %>% sum()
    systemic_index[i,"PD"]=PD %>% sum()
    systemic_index[i,"LGD"]=LGD %>% sum()
  }
  return(systemic_index)
    
}

# shocks=c("Commodity_crisis","Trade_crisis","Inflation_crisis")
# shocks=c("Severe_recession","Political_crisis","Wars")
# shocks=c("Financial_crisis","Banking_crisis","Contagion")
# shocks=c("Floating_exchange_rate")

dt1=systemic_ts(shocks=shocks,alpha=1,beta=1,gamma=1)
dt2=systemic_ts(shocks=shocks,alpha=1,beta=1,gamma=0.5)
dt3=systemic_ts(shocks=shocks,alpha=1,beta=1,gamma=0)

ggplot()+
  geom_line(aes(x=as.numeric(rownames(dt1)),y=dt1[,"crisis"],color="high complex"))+
  geom_line(aes(x=as.numeric(rownames(dt1)),y=dt2[,"crisis"],color="low complex"))+
  geom_line(aes(x=as.numeric(rownames(dt1)),y=dt3[,"crisis"],color="no complex"))+
  theme_minimal()



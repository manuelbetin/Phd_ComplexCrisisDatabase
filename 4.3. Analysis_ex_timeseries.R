
plot_index_crisis=function(data,ctry,var1,var2,var3){
  #head(mydata)
  
  mydata1=data %>% filter(iso3c==ctry) %>% mutate(Review_number=str_replace(str_replace(Review_number,"review","R "),"_",""))
  mydata_request=mydata1 %>% filter(type=="request")
  mydata_review=mydata1 %>% filter(type=="review")
  
  
  ggplot(data=mydata1)+
    geom_hline(yintercept = 0,color="black")+
    geom_vline(xintercept = mydata_review$Period,color="lightgrey")+
  #  geom_vline(xintercept = mydata_request$Period,color="black",linetype="dotted")+
    geom_vline(xintercept = mydata_request$startdate,color="red",linetype="dotted")+
    geom_vline(xintercept = mydata_request$enddate,color="green",linetype="dotted")+
    #geom_text(aes(x=Period,y=mean(get(var1))+4,label=Review_number),color="black",angle=90,size=3)+
    #geom_text(data=mydata_request,aes(x=Period,y=-2,label=type_program),color="black",angle=90,size=3)+
    
    geom_bar(stat="identity",aes(x=Period,y=get(var1)),fill="black",alpha=1)+
    geom_line(aes(x=Period,y=get(var1),color=str_replace_all(var1,"_norm","")))+
    #geom_smooth(aes(x=date,y=get(var1),color=str_replace_all(var1,"_norm","")),se=F)+
    geom_point(aes(x=Period,y=get(var1),color=str_replace_all(var1,"_norm","")),alpha=0.2)+
    
    geom_bar(stat="identity",aes(x=Period,y=get(var2)),fill="black",alpha=1)+
    #geom_smooth(aes(x=date,y=get(var2),color=str_replace_all(var2,"_norm","")),se=F)+
    geom_line(aes(x=Period,y=get(var2),color=str_replace_all(var2,"_norm","")))+
    geom_point(aes(x=Period,y=get(var2),color=str_replace_all(var2,"_norm","")),alpha=0.3)+
    
    geom_bar(stat="identity",aes(x=Period,y=get(var3)),fill="black",alpha=1)+
    #geom_smooth(aes(x=date,y=get(var3),color=str_replace_all(var3,"_norm","")),se=F)+
    geom_line(aes(x=Period,y=get(var3),color=str_replace_all(var3,"_norm","")))+
    geom_point(aes(x=Period,y=get(var3),color=str_replace_all(var3,"_norm","")),alpha=0.4)+
    
    theme_classic()+
    #lims(y=c(-2,6))+
    labs(x=NULL,y="st. dev.")+
    theme(legend.position = "bottom",legend.title=element_blank(),axis.text.x=element_text(angle=90,hjust=1))+
    scale_x_date(date_breaks = "1 year",date_labels =  "%Y")
}

# # 
#  data=mydata
#  ctry="MEX"
#  var1="Currency_crisis_norm"
#  var2="Currency_crisis_norm"
#  var3="Currency_crisis_norm"

my_ts_index=list()

# my_ts_index[["Financial"]]=plot_index_crisis(mydata,ctry,var1="Currency_crisis_norm",var2="Currency_crisis_norm",var3="Currency_crisis_norm")+
#   ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/ts_index_Financial_1",ctry,".png"))


dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country//Currency")
dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Spread")
dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Fiscal_outcomes")
dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Financial")
dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Monetary_policy")
dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Reforms")
dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Natural_disaster")

output[["TS_index_fig_example"]]=lapply(ctry,function(x){
  # my_ts_index[["Currency"]]=plot_index_crisis(mydata,x,var1="Reduction_reserves_norm",var2="Currency_crisis_norm",var3="Currency_crisis_severe_norm")#+
  # ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Currency/ts_index_Currency_",x,".png"))
  # 
  # my_ts_index[["Spread"]]=plot_index_crisis(mydata,x,var1="Contagion_norm",var2="Expectations_norm",var3="World_outcomes_norm")#+
  # ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Spread/ts_index_Spread_",x,".png"))
  # 
  # my_ts_index[["Fiscal_outcomes"]]=plot_index_crisis(mydata,x,var1="Sovereign_default_norm",var2="Fiscal_outcomes_norm",var3="Fiscal_consolidation_norm")#+
  # ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Fiscal_outcomes/ts_index_Fiscal_outcomes_",x,".png"))
  # 
  # my_ts_index[["Financial"]]=plot_index_crisis(mydata,x,var1="Banking_crisis_norm",var2="Financial_crisis_norm",var3="Contagion_norm")#+
  # ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Financial/ts_index_Financial_",x,".png"))
  # 
  # my_ts_index[["Reforms"]]=plot_index_crisis(mydata,x,var1="Political_crisis_norm",var2="Uncertainty_reforms_norm",var3="Social_crisis_norm")#+
  # ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Reforms/ts_index_Reforms_",x,".png"))
  # 
  # my_ts_index[["Monetary_policy"]]=plot_index_crisis(mydata,x,var1="Tightening_monetary_policy_norm",var2="Losening_monetary_policy_norm",var3="Inflation_crisis_norm")#+
  # ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Monetary_policy/ts_index_Monetary_policy_",x,".png"))

  my_ts_index[["Natural_disaster"]]=plot_index_crisis(mydata,x,var1="Natural_disaster",var2="Natural_disaster",var3="Natural_disaster")#+
  ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/Natural_disaster/ts_index_Natural_disaster_",x,".png"))
  
})

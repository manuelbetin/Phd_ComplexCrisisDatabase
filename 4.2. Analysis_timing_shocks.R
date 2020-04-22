
myvars=names(mydata)[str_detect(names(mydata),"_norm")]
res=mydata %>% #filter(iso3c=="URY") %>%
  group_by(review_number_new) %>% filter(!is.na(review_number_new) & review_number_new!="review" ) %>%
  summarize_at(vars(myvars),funs(mean(.,na.rm=T))) %>% gather(key='vars',value='value',-review_number_new)

subsamples=list(Exchange_rate=c("Currency_crisis_norm","Reduction_reserves_norm","Balance_payment_crisis_norm",
                                "Expectations_norm")
              , Output=c("Soft_recession_norm","Severe_recession_norm","Expansion_norm")
              , Financial=c("Banking_crisis_norm","Contagion_norm","Financial_crisis_norm","World_outcomes_norm")
              , Debt=c("Fiscal_outcomes_norm","Sovereign_default_norm","Fiscal_consolidation_norm")
              , Commodity=c("Commodity_crisis_norm","Natural_disaster_norm","trade_crisis_norm","Epidemics_norm")
              , Monetary_pol=c("Tightening_monetary_policy_norm",'Losening_monetary_policy_norm')
              , Political=c("Political_crisis_norm","Social_crisis_norm")
              , Structure=c("Short_term_debt_norm","Concessional_lending_norm","floating_rate_debt_norm","foreign_debt_norm")
              , Reforms=c("Trade_reforms_norm","Financial_reforms_norm","Labor_market_reforms_norm","Tax_reforms_norm","Banking_reforms_norm")
  )

vars=unlist(subsamples)
categories=data.frame(vars)
categories$category=str_remove(rownames(categories),"\\d")
res=res %>% left_join(categories, by="vars")
res=res %>% filter(!is.na(category))

output[["Timing_crisis_fig"]]=lapply(1:length(subsamples),function(i){
  x=subsamples[[i]]
  ggplot(res %>% filter(vars %in% x))+
    geom_hline(yintercept=0,color="black")+
    geom_point(stat="identity",aes(x=review_number_new,y=value,color=str_remove(vars,"_norm"),shape=str_remove(vars,"_norm")),size=4)+
    geom_line(stat="identity",aes(x=review_number_new,y=value,group=str_remove(vars,"_norm"),color=str_remove(vars,"_norm")),linetype="dashed",alpha=0.6)+
    labs(x=NULL,
         y='st.dev from the sample mean')+
    theme_bw()+
    theme(legend.position = "bottom",legend.title = element_blank())+
    ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Timing shocks/Timing_shocks_",names(subsamples)[[i]],".png"))
})

names(output[["Timing_crisis_fig"]])=names(subsamples)


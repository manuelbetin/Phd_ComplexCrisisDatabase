shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis','Reduction_reserves','Currency_crisis','Severe_recession','Soft_recession','Expansion',
         'Fiscal_outcomes','Sovereign_default',"Currency_crisis_severe")

corr_matrix=mydata %>% ungroup() %>%filter(type=="request")%>% dplyr::select(shocks) %>% cor() %>% data.frame()
#corr_matrix$var=names(corr_matrix)


#average correlation
avg_corr_matrix=corr_matrix %>% summarize_all(mean)
ggplot()+
  geom_bar(stat="identity",aes(x=reorder(x=names(avg_corr_matrix),as.numeric(avg_corr_matrix[1,])),y=as.numeric(avg_corr_matrix[1,])))+
  theme_bw()+
  theme(legend.position = "bottom",legend.title=element_blank(),axis.text.x=element_text(angle=90,hjust=1))


#correlation for selected shock
corr_matrix %>% dplyr::select(Currency_crisis)
var="Balance_payment_crisis"
ggplot()+
  geom_bar(stat="identity",aes(x=reorder(x=rownames(corr_matrix),as.numeric(corr_matrix[,var])),y=as.numeric(corr_matrix[,var])))+
  theme_bw()+
  theme(legend.position = "bottom",legend.title=element_blank(),axis.text.x=element_text(angle=90,hjust=1))

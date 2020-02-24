
max_no_1=function(x){
  max(ifelse(x!=1,x,NA),na.rm=T)
}
mean_no_1=function(x){
  mean(ifelse(x!=1,x,NA),na.rm=T)
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







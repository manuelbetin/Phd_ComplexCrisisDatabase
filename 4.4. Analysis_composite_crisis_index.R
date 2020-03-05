shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',
         'Severe_recession','Sovereign_default',"Currency_crisis_severe","Wars","Social_crisis")

Composite_crisis=plot_PIC_ts(mydata,shocks=shocks)
Composite_crisis+labs(title="Composite")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_crisis.png")

economic_crisis=plot_PIC_ts(mydata,shocks=c("World_outcomes","Severe_recession","Soft_recession"))
economic_crisis+labs(title="Economic")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Economic_crisis.png")

Financial_crisis=plot_PIC_ts(mydata,shocks=c("Financial_crisis","Banking_crisis","Contagion","Expectations"))
Financial_crisis+labs(title="Financial")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Financial_crisis.png")

Fiscal_crisis=plot_PIC_ts(mydata,shocks=c("Fiscal_outcomes","Fiscal_consolidation","Sovereign_default"))
Fiscal_crisis+labs(title="Fiscal")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Fiscal_crisis.png")

Exogeneous=plot_PIC_ts(mydata,shocks=c("Trade_crisis","Natural_disaster","Commodity_crisis"))
Exogeneous+labs(title="Exogeneous")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Exogeneous_crisis.png")

Currency=plot_PIC_ts(mydata,shocks=c("Currency_crisis","Inflation_crisis","Balance_payment_crisis","Contagion",
                                          'Expectations',"Reduction_reserves"))
Currency+labs(title="Currency")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Currency_crisis.png")

#composit index by type of countries
ctry_classif=SetUpProject::ctry_classif()%>% rename(iso3c=ISO3_Code)
ctry_groups=SetUpProject::ctry_groups()%>% rename(iso3c=ISO3_Code)

mydata=mydata %>% left_join(ctry_groups,by="iso3c")

#Hgh income countries
dt_highincome=mydata %>% filter(Income_group%in%c(" High income")& !is.na(Banking_crisis))
dt_highincome$iso3c %>% unique()
Composite_crisis_high_income=plot_PIC_ts(dt_highincome,
                                         shocks=shocks)+ labs(limits=c(-2,3))

#middle income countries
dt_middleincome=mydata %>% filter(Income_group%in%c(" Upper middle income",
                                                    " Lower middle income")& !is.na(Banking_crisis))
dt_middleincome$iso3c %>% unique()
Composite_crisis_middle_income=plot_PIC_ts(dt_middleincome,shocks=shocks)

#Low income countries
dt_lowincome=mydata %>% filter(Income_group%in%c(" Low income")& !is.na(Banking_crisis))
dt_lowincome$iso3c %>% unique()
Composite_crisis_low_income=plot_PIC_ts(dt_lowincome,shocks=shocks)

# Developped countries
dt_developped=mydata %>% filter(Classification%in%c("Developped")& !is.na(Banking_crisis))
dt_developped$iso3c %>% unique()
Composite_crisis_dev=plot_PIC_ts(dt_developped,
                                         shocks=shocks)+labs(title="Developped Countries")+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_index_Developped.png")

# Emerging countries
dt_Emerging=mydata %>% filter(Classification%in%c("Emerging")& !is.na(Banking_crisis))
dt_Emerging$iso3c %>% unique()

Composite_crisis_EM=plot_PIC_ts(dt_Emerging,
                                         shocks=shocks)+labs(title="Emerging Countries")+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_index_Emerging.png")


# Composite_crisis_high_income=plot_PIC_ts(dt_developped,
#                                          shocks=c("World_outcomes","Severe_recession","Soft_recession"))
# 
# 
# Composite_crisis_high_income=plot_PIC_ts(dt_developped,
#                                          shocks=c("Financial_crisis","Banking_crisis","Contagion","Expectations"))
# 
# 
# Composite_crisis_high_income=plot_PIC_ts(dt_developped,
#                                          shocks=c("Fiscal_outcomes","Fiscal_consolidation","Sovereign_default"))
# 
# Composite_crisis_high_income=plot_PIC_ts(dt_developped,
#                                          shocks=c("Trade_crisis","Natural_disaster","Commodity_crisis"))
# 
# Composite_crisis_high_income=plot_PIC_ts(dt_developped,
#                                          shocks=c("Currency_crisis","Inflation_crisis","Balance_payment_crisis","Contagion","Reduction_reserves"))
# 
# Composite_crisis_high_income=plot_PIC_ts(dt_developped,
#                                          shocks=c("Trade_crisis"))

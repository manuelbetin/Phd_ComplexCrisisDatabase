
# Index for all countries ####
All=list()

All[["All"]]=plot_PIC_ts(mydata,shocks=shocks)
All[["Composite"]]+labs(title="Composite")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_crisis.png")

All[["All"]][["economic_crisis"]]=plot_PIC_ts(mydata,shocks=c("World_outcomes","Severe_recession","Soft_recession"))
All[["economic_crisis"]]+labs(title="Economic")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Economic_crisis.png")

All[["Financial_crisis"]]=plot_PIC_ts(mydata,shocks=c("Financial_crisis","Banking_crisis_severe","Contagion","Expectations","Currency"))
All[["Financial_crisis"]]+labs(title="Financial")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Financial_crisis.png")

All[["Fiscal_crisis"]]=plot_PIC_ts(mydata,shocks=c("Sovereign_default","Sovereign_default","Sovereign_default"))
All[["Fiscal_crisis"]]+labs(title="Fiscal")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Fiscal_crisis.png")

All[["Exogeneous"]]=plot_PIC_ts(mydata,shocks=c("Trade_crisis","Natural_disaster","Commodity_crisis","Epidemics","Migration"))
All[["Exogeneous"]]+labs(title="Exogeneous")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Exogeneous_crisis.png")

All[["Currency"]]=plot_PIC_ts(mydata,shocks=c("Currency_crisis_severe","Inflation_crisis","Balance_payment_crisis","Contagion",
                                          'Expectations'))
All[["Currency"]]+labs(title="Currency")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Currency_crisis.png")

All[["Epidemics"]]=plot_PIC_ts(mydata,shocks=c("Epidemics"))
All[["Epidemics"]]+labs(title="Epidemics")+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Epidemics.png")



# Composit index by type of countries #####
ctry_classif=SetUpProject::ctry_classif()%>% rename(iso3c=ISO3_Code)
ctry_groups=SetUpProject::ctry_groups()%>% rename(iso3c=ISO3_Code)

mydata=mydata %>%
  left_join(ctry_groups,by="iso3c") %>%
  left_join(ctry_classif,by="iso3c")

PIC=list()
PIC["All"]=All

#Hgh income countries
dt_highincome=mydata %>% filter(Income_group%in%c(" High income")& !is.na(Banking_crisis))

PIC[["HighIncome"]]=list(Sample=dt_highincome$iso3c %>% unique(),
                                 Composite=plot_PIC_ts(dt_highincome,
                                         shocks=shocks)+ labs(limits=c(-2,3)))

#middle income countries
dt_middleincome=mydata %>% filter(Income_group%in%c(" Upper middle income",
                                                    " Lower middle income")& !is.na(Banking_crisis))
PIC[["MiddleIncome"]]=list(Sample=dt_middleincome$iso3c %>% unique(),
                                   Composite=plot_PIC_ts(dt_middleincome,shocks=shocks))

#Low income countries
dt_lowincome=mydata %>% filter(Income_group%in%c(" Low income")& !is.na(Banking_crisis))
PIC[["LowIncome"]]=list(Sample=dt_lowincome$iso3c %>% unique(),
                                Composite=plot_PIC_ts(dt_lowincome,shocks=shocks))

# Developped countries
dt_developped=mydata %>% filter(Classification%in%c("Developped")& !is.na(Banking_crisis))

PIC[["DevCountries"]]=list(Sample=dt_developped$iso3c %>% unique(),
                                   Composite=plot_PIC_ts(dt_developped,
                                         shocks=shocks)+labs(title="Developped Countries")+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_index_Developped.png"))

# Emerging countries
dt_Emerging=mydata %>% filter(Classification%in%c("Emerging")& !is.na(Banking_crisis))

PIC[["EmCountries"]]=list(Sample=dt_Emerging$iso3c %>% unique(),
                                  Composite=plot_PIC_ts(dt_Emerging,
                                         shocks=shocks)+labs(title="Emerging Countries")+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_index_Emerging.png"))



# Epidemics_high=plot_PIC_ts(dt_highincome ,shocks=c("Epidemics"))
# Epidemics_middle=plot_PIC_ts(dt_middleincome ,shocks=c("Epidemics"))
# Epidemics_low=plot_PIC_ts(dt_lowincome ,shocks=c("Epidemics"))
# Epidemics_em=plot_PIC_ts(dt_Emerging ,shocks=c("Epidemics"))


output[["PIC_index"]]=list(All=All,
                           PIC=PIC)

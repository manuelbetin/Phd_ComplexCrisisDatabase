
# Index for all countries ####
All=list()

All[["All"]]=plot_PIC_ts(mydata,shocks=shocks)
All[["All"]]+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Composite_crisis.png")

All[["economic_crisis"]]=plot_PIC_ts(mydata,shocks=c("World_outcomes","Severe_recession","Soft_recession"))
All[["economic_crisis"]]+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Economic_crisis.png")

All[["Financial_crisis"]]=plot_PIC_ts(mydata,shocks=c("Financial_crisis","Banking_crisis","Contagion","Expectations","Currency_crisis_severe"))
All[["Financial_crisis"]]+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Financial_crisis.png")

All[["Fiscal_crisis"]]=plot_PIC_ts(mydata,shocks=c("Sovereign_default","Sovereign_default","Sovereign_default"))
All[["Fiscal_crisis"]]+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Fiscal_crisis.png")

All[["Exogeneous"]]=plot_PIC_ts(mydata,shocks=c("Trade_crisis","Natural_disaster","Commodity_crisis","Epidemics","Migration","Wars"))
All[["Exogeneous"]]+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Exogeneous_crisis.png")

All[["Currency"]]=plot_PIC_ts(mydata,shocks=c("Currency_crisis_severe","Inflation_crisis","Balance_payment_crisis","Contagion",
                                          'Expectations'))
All[["Currency"]]+ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/Currency_crisis.png")

# Composit index by type of countries #####

PIC=list()
PIC["All"]=All

#Hgh income countries

ctries=ctry_groups %>% filter(Income_group==" High income")
PIC[["HighIncome"]]=list(Sample=ctries$iso3c,
                                 Composite=plot_PIC_ts(mydata %>% filter(iso3c %in% ctries$iso3c),
                                         shocks=shocks)+ labs(limits=c(-2,3)))
PIC[["HighIncome"]]$Composite+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/composite_HighIncome.png")


#middle income countries
ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
PIC[["MiddleIncome"]]=list(Sample=ctries$iso3c,
                           Composite=plot_PIC_ts(mydata %>% filter(iso3c %in% ctries$iso3c),
                                                 shocks=shocks)+ labs(limits=c(-2,3)))
PIC[["MiddleIncome"]]$Composite+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/composite_MiddleIncome.png")

#Low income countries
ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
PIC[["LowIncome"]]=list(Sample=ctries$iso3c,
                        Composite=plot_PIC_ts(mydata %>% filter(iso3c %in% ctries$iso3c),
                                              shocks=shocks)+ labs(limits=c(-2,3)))
PIC[["LowIncome"]]$Composite+
  ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Systemic index/composite_LowIncome.png")


output[["PIC_index"]]=list(All=All,
                           PIC=PIC)

output$PIC_index$All$Currency



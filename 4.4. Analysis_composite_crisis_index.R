
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


# Script for first results on output losses.
# We use the local projections method described by Jorda (2005) to estimate output losses with our created indexes.

library(lpirfs)

# Let's normalize the indexes we are interested in:

indexes_2normalize <- c("Currency_crisis_severe","Soft_recession","Severe_recession")


annual_tf_idf_norm <- annual_tf_idf %>% 
  ungroup () %>%
  mutate_at(vars(indexes_2normalize), funs(norm = (. - mean(.,na.rm=T))/sd(.,na.rm=T)))

# Local projections: 

lp_df <- annual_tf_idf_norm %>% select(ISO3_Code, year, Currency_crisis_severe_norm, Soft_recession_norm, Severe_recession_norm)

# Figure 3 Romer & Romer:

# Without lags explanatory variable:


results_panel1 <- lp_lin_panel(annual_tf_idf_norm, 
             endog_data = "Severe_recession_norm",
             shock = "Currency_crisis_severe_norm",
             panel_model = "within",
             panel_effect = "twoways",
             confint = 1.96,
             hor = 5
       )

plot(results_panel1)

# With lags explanatory variable:

results_panel2 <- lp_lin_panel(annual_tf_idf_norm, 
                               endog_data = "Severe_recession_norm",
                               shock = "Currency_crisis_severe_norm",
                               panel_model = "within",
                               panel_effect = "twoways",
                               l_exog_data = "Currency_crisis_severe",
                               lags_exog_data = 2,
                               confint = 1.96,
                               hor = 5
)

plot(results_panel2)

# Figure 5 Romer & Romer:

results_panel2 <- lp_lin_panel(annual_tf_idf_norm, 
                               endog_data = "Currency_crisis_severe_norm",
                               shock = "Currency_crisis_severe_norm",
                               panel_model = "within",
                               panel_effect = "twoways",
                               confint = 1.96,
                               hor = 30
)

plot(results_panel2)



# what it means that is never dying?




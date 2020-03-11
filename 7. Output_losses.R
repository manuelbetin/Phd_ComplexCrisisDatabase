# Script for first results on output losses.
# We use the local projections method described by Jorda (2005) to estimate output losses with our created indexes.
# Download real gdp from world bank.

library(lpirfs)
library(wbstats)

# Let's normalize the indexes we are interested in:

indexes_2normalize <- c("Currency_crisis_severe","Soft_recession","Severe_recession")


annual_tf_idf_norm <- annual_tf_idf %>% 
  ungroup () %>%
  mutate_at(vars(indexes_2normalize), funs(norm = (. - mean(.,na.rm=T))/sd(.,na.rm=T))) 

# Download World Bank data:

growth_df <- wb(country = "countries_only",indicator = "NY.GDP.MKTP.KD.ZG",startdate = 1954, enddate = 2016) %>% 
  rename(ISO3_Code = iso3c, 
         year = date,
         gdp_growth = value) %>% 
  select(ISO3_Code, year, gdp_growth)

# Use country group classification:

classification <- import("../Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData") %>% 
  select(ISO3_Code,Income_group,group) %>% 
  filter(!duplicated(ISO3_Code))


# Merge final df:

lp_df <- merge(annual_tf_idf_norm,
               growth_df,
               by = c("ISO3_Code","year"),
               all.x = TRUE) %>% 
  merge(classification, by=c("ISO3_Code"), all.x = TRUE)


# Local projections: 

lp_df <- lp_df %>% select(ISO3_Code, year, Currency_crisis_severe_norm, Soft_recession_norm, Severe_recession_norm, gdp_growth,
                          Income_group, group) %>% 
  arrange(ISO3_Code, year)

# Figure 3 Romer & Romer: ------

# Without lags explanatory variable:

dependent_var <- c("Soft_recession_norm", "Severe_recession_norm")

results1 <- dependent_var %>% 
  map( ~ lp_lin_panel(annual_tf_idf_norm, 
             endog_data = .x,
             shock = "Currency_crisis_severe_norm",
             panel_model = "within",
             panel_effect = "twoways",
             confint = 1.96,
             hor = 5
       )
  )

# soft
plot(results1[[1]])
# severe
plot(results1[[2]])

# With lags explanatory variable:

results2 <- dependent_var %>% 
  map( ~ lp_lin_panel(annual_tf_idf_norm, 
                               endog_data = .x,
                               shock = "Currency_crisis_severe_norm",
                               panel_model = "within",
                               panel_effect = "twoways",
                               l_exog_data = "Currency_crisis_severe",
                               lags_exog_data = 2,
                               confint = 1.96,
                               hor = 5
)
)

# soft
plot(results2[[1]])
# severe
plot(results2[[2]])

# Figure 5 Romer & Romer: -----

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

# Possible nonlinearities: -----

results_panel4 <- lp_nl_panel(annual_tf_idf_norm, 
                               endog_data = "Severe_recession_norm",
                               shock = "Currency_crisis_severe_norm",
                               panel_model = "within",
                               panel_effect = "twoways",
                               l_exog_data = "Currency_crisis_severe",
                               lags_exog_data = 2,
                               confint = 1.96,
                               hor = 5,
                               switching = "Currency_crisis_severe_norm",
                                gamma = 0.2
)


# Figure 3 Romer & Romer (with actual gdp growth)

lp_df_emerging <- lp_df %>% filter(group == "Emerging countries" | ISO3_Code == "ARG")
lp_df_developed <- lp_df %>% filter(group == "Developped countries" & ISO3_Code != "ARG")

results_panel5 <- lp_lin_panel(lp_df_emerging,
             endog_data = "gdp_growth",
             shock = "Currency_crisis_severe_norm",
             panel_model = "within",
             panel_effect = "individual",
             confint = 1.68,
             h = 3)

plot(results_panel5)





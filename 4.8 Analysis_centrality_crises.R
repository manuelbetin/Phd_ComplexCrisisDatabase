################### Description: script to analyze how each crisis moves 
################### within the macroeconomic system during the last 70 years i.e. centrality of the shocks.
################### 
library(igraph)
library(gganimate)


# Pre-process: ------
# Creation of a nested list with income group first level and time bucket second level

# Import: 

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year))%>% 
  select(-Soft_recession, -Banking_crisis) %>% 
  mutate_at(vars(Epidemics:World_outcomes), funs(norm = (. - mean(.,na.rm=T))/sd(.,na.rm=T))) %>% 
  ungroup() 

# Classification data:


income_groups <- c("High income","Upper middle income","Low income")

classification <- import("../Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData") %>% 
  select(ISO3_Code,Income_group,group) %>% 
  filter(!duplicated(ISO3_Code)) %>% 
  mutate(Income_group = ifelse(Income_group == "Lower middle income","Low income",Income_group))

# Final and different income groups df:

mydata <- mydata %>% 
merge(classification) 

mydata_income <- income_groups %>% 
  map(~ mydata %>% filter(Income_group == .x)) 

names(mydata_income) <- income_groups


# Select normalized variable and create time buckets

vars_norm <- vars_select(names(mydata), ends_with('norm'))

buckets <- list(
  `1950:1976` = 1950:1976, 
  `1976:1992` = 1976:1992,
  `1992:2003` = 1992:2003,
  `2003:2013` = 2003:2013,
  `2013:2020` = 2013:2020
)

# Nested list:

mydata <- mydata %>% mutate(bucket = case_when(year >= 1950 & year <= 1976 ~ "1950:1976",
                                         year >= 1976 & year <= 1992 ~ "1976:1992",
                                         year >= 1992 & year <= 2003 ~ "1992:2003",
                                         year >= 2003 & year <= 2019 ~ "2003:2019"))

mydata <- split(mydata,mydata$bucket)
                  

final <- mydata_income %>% 
  map(~ .x %>% mutate(bucket = case_when(year >= 1950 & year <= 1976 ~ "1950:1976",
                                         year >= 1976 & year <= 1992 ~ "1976:1992",
                                         year >= 1992 & year <= 2003 ~ "1992:2003",
                                         year >= 2003 & year <= 2013 ~ "2003:2013",
                                         year >= 2013 & year <= 2019 ~ "2013:2019"
  ))) %>% 
  map(~ split(.x, .x$bucket))



# Creation different adjancies matrices: -----

# Regression lag-1 variable - first step, list with 4 buckets elements
# For each bucket, each variable lagged and all other variables.

reg <- buckets %>% 
  map(~ mydata %>% filter(year %in% .x)) %>% 
  map(~ .x %>% mutate_if(is.numeric,get_prob)) %>% 
  map(~ .x %>% select(Epidemics:World_outcomes)) %>% 
  map(~ .x %>% mutate_all(funs(lag = dplyr::lag(.,1)))) %>% 
  map(~ .x %>% gather("lag","value", Epidemics_lag:World_outcomes_lag)) %>% 
  map(~ split(.x,.x$lag)) %>% 
  modify_depth(2, ~ .x %>% select(-lag)) 

# We have to figure out how to regress each lagged variable on all others.


reg[[1]] %>% 
  map(function(x){
    map(x, ~ lm(.x ~ value, x) %>% summary())
  })


reg[[2]] %>% 
  map(function(x){
    map(x, ~ lm(.x ~ value, x))
  })


reg[[3]] %>% 
  map(function(x){
    map(x, ~ lm(.x ~ value, x))
  })


reg[[4]] %>% 
  map(function(x){
    map(x, ~ lm(.x ~ value, x) %>% summary())
  })

# Correlation adjacency matrix:

corr <- mydata%>% 
  map(~ .x %>% select(vars_norm)) %>% 
  map(~ .x %>% cor(use = "complete.obs"))

corr_final <- final %>%  
  modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
  modify_depth(2, ~ .x %>% cor(use = "complete.obs"))



# Calculation eigencentrality by time bucket: -----


network <- corr_final %>% 
  modify_depth(2, ~ graph_from_adjacency_matrix(.x, mode = "undirected",diag = F, weighted = T))


centrality <- network %>% 
  modify_depth(2, ~ eigen_centrality(.x)$vector) %>% 
  modify_depth(2, ~ .x %>% stack()) %>% 
  modify_depth(2, ~ .x %>% rename(eigencentrality = values, category = ind)) %>% 
  modify_depth(2, ~ .x %>% select(category, everything())) %>% 
  modify_depth(2, ~ .x %>% arrange(-eigencentrality)) %>% 
  modify_depth(2,~ .x %>% mutate(category = str_remove(category,"_norm"))) %>% 
  modify_depth(2, ~ .x %>% mutate(category = str_replace_all(category,"_"," ")))


# Let's try to do it similar to Manu's intensity:

 



# Same thing with animation:

animated_df <- centality %>%
  map(~ .x %>% mutate(category = fct_reorder(category, eigencentality))) %>% 
  bind_rows(.id = "Time span") 

plot <- animated_df %>% 
        ggplot(aes(category, eigencentality, fill= type)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        ylab("") +
        xlab("") +
        labs(fill = "Type of shock:")

plot + transition_states(`Time span`, state_length = 6, transition_length = 4)






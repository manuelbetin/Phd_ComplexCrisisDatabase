################### Description: script to analyze how each crisis moves 
################### within the macroeconomic system during the last 70 years i.e. centrality of the shocks.
################### 
library(igraph)
library(gganimate)


# Pre-process: ------

# Import

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year))%>% 
  select(-Soft_recession, -Banking_crisis) %>% 
  mutate_at(vars(Epidemics:World_outcomes), funs(norm = (. - mean(.,na.rm=T))/sd(.,na.rm=T))) %>% 
  ungroup() 

income_groups <- c("High income","Upper middle income","Low income")

classification <- import("../Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData") %>% 
  select(ISO3_Code,Income_group,group) %>% 
  filter(!duplicated(ISO3_Code)) %>% 
  mutate(Income_group = ifelse(Income_group == "Lower middle income","Low income",Income_group))

mydata <- mydata %>% 
merge(classification) 

mydata_income <- income_groups %>% 
  map(~ mydata %>% filter(Income_group == .x))



# Select normalized variable and create time buckets

vars_norm <- vars_select(names(mydata), ends_with('norm'))

buckets <- list(
  `1950:1976` = 1950:1976, 
  `1976:1992` = 1976:1992,
  `1992:2003` = 1992:2003,
  `2003:2013` = 2003:2013,
  `2013:2020` = 2013:2020
)

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


corr <- buckets %>% 
  map(~ mydata %>% filter(year %in% .x)) %>% 
  map(~ .x %>% select(vars_norm)) %>% 
  map(~ .x %>% cor(use = "complete.obs"))



# Calculation eigencentrality by time bucket: -----


network <- corr %>% 
  map(~ graph_from_adjacency_matrix(.x, mode = "undirected",diag = F, weighted = T))



network %>% 
map(~ plot(.x))


centality <- network %>% 
  map(~ eigen_centrality(.x)$vector) %>% 
  map(~ .x %>% stack()) %>% 
  map(~ .x %>% rename(eigencentality = values, category = ind)) %>% 
  map(~ .x %>% select(category, everything())) %>% 
  map(~ .x %>% arrange(-eigencentality)) %>% 
  map(~ .x %>% mutate(category = str_remove(category,"_norm"))) %>% 
  map(~ .x %>% mutate(category = str_replace_all(category,"_"," "))) %>% 
  map(~ .x %>% mutate(type = case_when(category == "Wars"| category == "Natural disaster" | category == "Epidemics" | category == "Migration" | category == "Social crisis" ~ "Non economic",
                                       TRUE ~ "Economic")))

centality %>%
  map(~ .x %>% mutate(category = fct_reorder(category, eigencentality))) %>% 
  map(~ .x %>% ggplot(aes(category, eigencentality, fill= eigencentality)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        ylab("") +
        xlab("") +
        scale_fill_gradient(low = "white",high = "red")+
        labs(fill = "Eigencentrality"))


# Let's try to do it similar to Manu's intensity:


centality %>% 
  bind_rows(.id = "period") %>% 
  ggplot(aes(fill=period, y=eigencentality, x=category)) + 
  geom_bar(position="stack", stat="identity", col = "black", alpha = 0.9) +
  theme_minimal() +
  theme(axis.text.x = element_text(size =14,angle=90), axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        legend.title = element_blank()) +
  scale_fill_grey() +
  scale_color_grey() +
  ylab("Eigencentrality")



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






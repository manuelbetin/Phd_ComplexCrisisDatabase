# Centrality of the shocks ----

library(igraph)

# Import data:


mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year))%>% 
  mutate_at(vars(Epidemics:World_outcomes), funs(norm = (. - mean(.,na.rm=T))/sd(.,na.rm=T))) %>% 
  ungroup() 

vars_norm <- vars_select(names(mydata_norm), ends_with('norm'))

# Creation edges:

buckets <- list(
  `1950:1975` = 1950:1975, 
  `1975:1990` = 1975:1990,
  `1990:2005` = 1990:2005,
  `2005:2019` = 2005:2020)

# Creation different adjancies matrices:

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



corr <- buckets %>% 
  map(~ mydata %>% filter(year %in% .x)) %>% 
  map(~ .x %>% select(vars_norm)) %>% 
  map(~ .x %>% cor(use = "complete.obs"))



# Calculation eigencentrality:

library(igraph)

network <- corr %>% 
  map(~ graph_from_adjacency_matrix(.x, mode = "undirected",diag = F, weighted = T))

network %>% 
map(plot)


centality <- network %>% 
  map(~ eigen_centrality(.x)$vector) %>% 
  map(~ .x %>% stack()) %>% 
  map(~ .x %>% rename(eigencentality = values, category = ind)) %>% 
  map(~ .x %>% select(category, everything())) %>% 
  map(~ .x %>% arrange(-eigencentality)) %>% 
  map(~ .x %>% mutate(category = str_remove(category,"_norm"))) %>% 
  map(~ .x %>% mutate(category = str_replace(category,"_"," "))) %>% 
  map(~ .x %>% mutate(type = case_when(category == "Wars"| category == "Natural disaster" | category == "Epidemics" | category == "Migration" | category == "Social crisis" ~ "Non economic",
                                       TRUE ~ "Economic")))

centality %>%
  map(~ .x %>% mutate(category = fct_reorder(category, eigencentality))) %>% 
  map(~ .x %>% ggplot(aes(category, eigencentality, fill= type)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        ylab("") +
        xlab("") +
        labs(fill = "Type of shock:"))



# Animation:

library(gganimate)
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






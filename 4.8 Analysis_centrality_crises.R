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


corr <- buckets %>% 
  map(~ mydata %>% filter(year == .x)) %>% 
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



















only_normalized <- data_for_pca %>% 
  select(ISO3_Code, year, vars_norm) 

pr.out <- prcomp(na.omit(only_normalized),scale = F)

pr.var =pr.out$sdev ^2 
(pr.var[1]/sum(pr.var))*100

pr.var
str(pr.out)  
pr.out$rotation= -pr.out$rotation 

pr.out$x=-pr.out$x 



biplot (pr.out , scale =0)

pr.out$rotation
pca_indexes$var$contrib


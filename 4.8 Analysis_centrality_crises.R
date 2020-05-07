################### Description: script to analyze how each crisis moves 
################### within the macroeconomic system during the last 70 years i.e. centrality of the shocks and
################### evolution of whole system
library(igraph)
library(gganimate)
library(networkD3)


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


# Select normalized variable 

vars_norm <- vars_select(names(mydata), ends_with('norm'))


# Lists:
# All countries, list with elements time buckets dfs

mydata <- mydata %>% mutate(bucket = case_when(year >= 1950 & year <= 1976 ~ "1950:1976",
                                         year >= 1976 & year <= 1992 ~ "1976:1992",
                                         year >= 1992 & year <= 2003 ~ "1992:2003",
                                         year >= 2003 & year <= 2013 ~ "2003:2012",
                                         year >= 2013 & year <= 2019 ~ "2013:2019"))

mydata <- split(mydata,mydata$bucket)
    
# Nested list: first element income group, second time bucket

final <- mydata_income %>% 
  map(~ .x %>% mutate(bucket = case_when(year >= 1950 & year <= 1976 ~ "1950:1976",
                                         year >= 1976 & year <= 1992 ~ "1976:1992",
                                         year >= 1992 & year <= 2003 ~ "1992:2003",
                                         year >= 2003 & year <= 2013 ~ "2003:2013",
                                         year >= 2013 & year <= 2019 ~ "2013:2019"
  ))) %>% 
  map(~ split(.x, .x$bucket))



# Network evolution general characteristics: -----
###### Degree - number of links in the network

# Set function - in correlation matrix exclude all values less than a minimum

set_threshold <- function(x,min_cor = 0.2){
  ifelse(x > min_cor, x, 0)
}

# Param for vectorization over different minima - change at need

vector_min_cor <- c(0.2,0.3,0.4)

# Dataframe creation:

corr_final <- vector_min_cor %>% 
  map(function(y){
    final %>%  
    modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
    modify_depth(2, ~ .x %>% cor(use = "complete.obs")) %>% 
    modify_depth(2, ~ .x[lower.tri(.x, diag = F)]) %>% 
    modify_depth(2, ~ data.frame(links = .x) %>% mutate_all(set_threshold,y) %>% filter(links != 0)) %>% 
    modify_depth(2, ~ .x %>% count()) %>% 
    map(~ bind_rows(.x,.id = "period")) %>% 
    bind_rows(.id = "group")}
    ) %>% 
  map2(vector_min_cor, ~ .x %>% mutate(min_cor = .y)) %>% 
  bind_rows()
  
# Plot for a single value of minimum correlation:

corr_final %>%
  filter(min_cor == 0.2) %>% 
  ggplot(aes(period, n, col = group, group = 1)) +
  geom_line() +
  facet_wrap(~ group) +
  theme_minimal() +
  scale_color_grey() +
  xlab("") +
  ylab("Number of edges") +
  theme(legend.position = "none", 
        axis.text=element_text(size=14), axis.text.x = element_text(size =14,angle=90),
        axis.title.y = element_text(size=14),
        strip.text = element_text(face="bold", size=14))
  

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Complexity/Evolution/complexity_evolution.png",
       dpi = "retina")


# Table:


corr_final %>% 
  spread("period","n") %>% 
  rename(`Income group` = group, `Min. Corr.` = min_cor) %>% 
  stargazer(summary = F, out = "../Betin_Collodel/2. Text mining IMF_data/output/tables/Complexity/Evolution/complexity_evolution.tex")


#### Average path length

avg_path_length <- vector_min_cor %>% 
  map(function(y){
  final %>%  
      modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
      modify_depth(2, ~ .x %>% cor(use = "complete.obs")) %>% 
      modify_depth(2, ~ ifelse(.x < y, 0, .x)) %>% 
      modify_depth(2, ~ graph_from_adjacency_matrix(.x, mode = "undirected", diag = F, weighted = T)) %>%
      modify_depth(2, ~ mean_distance(.x, unconnected = F)) %>% 
      map(~ bind_rows(.x)) %>% 
      bind_rows(.id = "Income group")}) %>% 
  map2(vector_min_cor, ~ .x %>% mutate(min_cor = .y)) %>% 
  bind_rows()

avg_path_length %>% 
  mutate_if(is.double, round, 2) %>%
  rename(`Min. Corr` = min_cor) %>% 
  select(`Income group`,`Min. Corr`,everything()) %>% 
  arrange(`Income group`) %>% 
  stargazer(summary = F, out = "../Betin_Collodel/2. Text mining IMF_data/output/tables/Complexity/Evolution/average_path_length.tex")

#### Degree distribution

degree_distribution <- vector_min_cor %>% 
  map(function(y){
  final %>%  
  modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
  modify_depth(2, ~ .x %>% cor(use = "complete.obs")) %>% 
  modify_depth(2, ~ ifelse(.x < y, 0, .x)) %>% 
  modify_depth(2, ~ graph_from_adjacency_matrix(.x, mode = "undirected", diag = F, weighted = T)) %>%
  modify_depth(2, ~ degree(.x)) %>% 
  modify_depth(2, ~ stack(.x)) %>% 
  modify_depth(2, ~ hist(.x$values, plot = F)$count) %>% 
  modify_depth(2, ~ kurtosis(.x)) %>% 
  map(~ bind_rows(.x)) %>% 
  bind_rows(.id = "Income group")}) %>% 
  map2(vector_min_cor, ~ .x %>% mutate(min_cor = .y)) %>% 
  bind_rows()
  
degree_distribution %>% 
  mutate_if(is.double, round, 2) %>% 
  rename(`Min. Corr` = min_cor) %>% 
  select(`Income group`,`Min. Corr`,everything()) %>% 
  arrange(`Income group`) %>% 
  stargazer(summary = F, out = "../Betin_Collodel/2. Text mining IMF_data/output/tables/Complexity/Evolution/degree_distribution.tex")




# Calculation eigencentrslity by time bucket (all countries): ------

network <- mydata %>% 
  map(~ .x %>% select(vars_norm)) %>% 
  map(~ .x %>% cor(use = "complete.obs")) %>% 
  map(~ .x %>% graph_from_adjacency_matrix(mode = "undirected", diag = F, weighted = T))


centrality <- network %>%
  map(~ eigen_centrality(.x)$vector) %>% 
  map(~ .x %>% stack()) %>% 
  map(~ .x %>% rename(eigencentrality = values, category = ind)) %>% 
  map(~ .x %>% select(category, everything())) %>% 
  map(~ .x %>% arrange(-eigencentrality)) %>% 
  map(~ .x %>% mutate(category = str_remove(category,"_norm"))) %>% 
  map(~ .x %>% mutate(category = str_replace_all(category,"_"," ")))


centrality %>% 
  bind_rows(.id = "period") %>% 
        ggplot(aes(period, category, fill= eigencentrality, alpha = eigencentrality)) +
        geom_tile(col = "black") +
        theme_minimal() +
        ylab("") +
        xlab("") +
        labs(fill = "Eigencentrality") +
        theme(axis.text.x = element_text(size =14,angle=90, vjust=0.5, hjust=1), axis.text.y = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              legend.position = "none") +
        scale_fill_gradient(low = "white",high = "red") +
        coord_fixed(ratio = .6)

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Complexity/Eigencentrality/Eigencentrality_All.png",
       height = 4,
       width = 5,
       dpi = "retina")

# Calculation eigencentrality by income group and time bucket: -----

corr_final <- final %>% 
      modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
      modify_depth(2, ~ .x %>% cor(use = "complete.obs"))

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


# Heatmap plot:

heatmap_eigencentrality <- centrality %>% 
  map(~ .x) %>% 
  map(~ bind_rows(.x, .id = "period")) %>% 
  map(~ .x %>% 
        ggplot(aes(period, category, fill= eigencentrality, alpha = eigencentrality)) +
        geom_tile(col = "black") +
        theme_minimal() +
        ylab("") +
        xlab("") +
        labs(fill = "Eigencentrality") +
        theme(axis.text.x = element_text(size =14,angle=90,vjust=0.5, hjust=1), axis.text.y = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              legend.position = "none") +
        scale_fill_gradient(low = "white",high = "red") +
        coord_fixed(ratio = .6)
)

heatmap_eigencentrality %>% 
  map2(names(heatmap_eigencentrality), ~ ggsave(paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Complexity/Eigencentrality/Eigencentrality_",.y,".png"),
               plot = .x,
               height = 4,
               width = 5,
               dpi = "retina"))



# Distribution network. ranking is not the same with highly distributed network and uniform one.
# Unfortunately, the analogy of snapshots to a motion picture also reveals the main difficulty with this approach: the time steps employed are very rarely suggested by the network and are instead arbitrary. 
# Using extremely small time steps between each snapshot preserves resolution, but may actually obscure wider trends which only become visible over longer timescales. 
# Conversely, using larger timescales loses the temporal order of events within each snapshot
  
  
# Interesting that for middle income it seems not stable over time. Investigate more on this:

centrality %>% 
  map(~ .x) %>% 
  map(~ bind_rows(.x, .id = "period")) %>% 
  map(~ .x %>% group_by(category) %>% summarise(sum_eigen =sum(eigencentrality))) %>% 
  bind_rows(.id = "Income group") %>% 
  ggplot(aes(x = sum_eigen, fill = `Income group`, alpha = 0.5)) +
  geom_density() +
  theme_minimal()
  

centrality %>% 
  map(~ .x) %>% 
  map(~ bind_rows(.x, .id = "period")) %>% 
  map(~ .x %>% group_by(category) %>% summarise(sum_eigen =sum(eigencentrality))) %>% 
  bind_rows(.id = "Income group") %>% 
  group_by(`Income group`) %>% 
  summarise(stability = mean(sum_eigen, na.rm = T))

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






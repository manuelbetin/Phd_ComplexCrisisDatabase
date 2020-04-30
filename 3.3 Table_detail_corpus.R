######## Description: the script generates 
######## 1) a histogram with the division of the corpus into two big families, country reports and programs, and granular ranking all in the same facet.
######## 2) number of documents for each year, family and type doc

# Remove problematic documents:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData") %>% 
  filter(!is.na(Wars) & !is.na(Currency_crisis_severe))

unique(mydata$type)

# We identify two big groups: country reports and program related documents.


group_data <- mydata %>% 
  mutate(family = case_when(type == "request"|type == "review"|type == "cancelation"|
                            type == "modification" ~ "Program related",
                           type == "technical assistance" ~ "Program related",
                           type == "consultation" ~ "Country reports",
         is.na(type) ~ "Country reports"))


# Detail corpus graph: ----

# Total country reports and program related docs:

total <- group_data %>%
  group_by(family) %>% 
  count() %>% 
  mutate(type_doc = "Total") %>% 
  ungroup()

# Granular ranking:

group_data %>% 
  group_by(family, type_doc) %>%
  count() %>% 
  ungroup() %>%
  rbind(total) %>% 
  mutate(type_doc = fct_reorder(type_doc, n)) %>% 
  ggplot(aes(type_doc, n, fill = family)) +
  geom_col(width = .3) +
  geom_text(aes(label=n), vjust= 0.5,  hjust = -0.2, size=4)+
  facet_wrap(~ family, ncol = 1, scales = "free_y") +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=13)) +
  theme(strip.text = element_text(face="bold", size=13))
        

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/corpus_detail.png",
       dpi = "retina")


# Evolution graphs: -----

by_year <- group_data %>% 
  group_by(family, year) %>% 
  count() 
  
family <- c("Country reports","Program related")
color <- c("#F8766D","#00BFC4")

evolution_graphs <- family %>% 
map2(color,function(x,y){
  group_data %>% 
  group_by(family,type_doc, year) %>% 
  count() %>%
  filter(family == x) %>% 
  ggplot(aes(year,n)) +
  geom_col(fill = y) +
  facet_wrap(~ type_doc) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(size=14), axis.text.x = element_text(size =14,angle=90)) +
  theme(strip.text = element_text(face="bold", size=13)) +
  labs(fill = "")}
)

names(evolution_graphs) <- family

evolution_graphs %>% 
map(~ ggsave(paste0( "../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/corpus_evolution/",.x,".png"),
      plot = .x,
      dpi = "retina")
)



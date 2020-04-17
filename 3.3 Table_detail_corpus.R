# Description: construction table with cleaned corpus detail

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData") 
unique(mydata$type_doc)

# We identify three big groups: programs related, consultations and technical assistance.


group_data <- mydata %>% 
  mutate(family = case_when(type == "request"|type == "review"|type == "cancelation"|
                            type == "modification" ~ "Program related",
                           type == "technical assistance" ~ "Technical assistance",
                           type == "consultation" ~ "Consultations")) 


group_data %>% 
  filter(!is.na(family)) %>% 
  group_by(family) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(family = factor(family, levels = c("Technical assistance", "Program related","Consultations"))) %>%
  ggplot(aes(family, n, fill = family)) +
  geom_col(width = .2) +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "none")


ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/corpus_detail.png")


# Build correspondent graph with subgroups by family.


# Description: construction table and graphs with final corpus detail

# Remove problematic documents:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData") %>% 
  filter(!is.na(Wars) & !is.na(Currency_crisis_severe))

unique(mydata$type)

# We identify three big groups: programs related, consultations and technical assistance.


group_data <- mydata %>% 
  mutate(family = case_when(type == "request"|type == "review"|type == "cancelation"|
                            type == "modification" ~ "Program related",
                           type == "technical assistance" ~ "Technical assistance",
                           type == "consultation" ~ "Consultations",
         is.na(type) ~ "Consultations"))

unique(group_data$family)

group_data %>% 
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


# Build correspondent graph with subgroups by family ----

group_data %>% 
  group_by(family, type_doc) %>%
  count() %>% 
  ungroup() %>% 
  mutate(type_doc = fct_reorder(type_doc, n)) %>% 
  ggplot(aes(type_doc, n, fill = family)) +
  geom_col(width = .3) +
  facet_wrap(~ family, ncol = 1, scales = "free_y") +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "none") 

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/corpus_detail2.png")

  

  
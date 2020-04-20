######## Description: the script generates a histogram with the division of the 
######## corpus into two big families, country reports and programs. A second graph
######## shows for each family the granular ranking of different types of documents.
######## To add: table with short explanation each type of document, whether dismissed or not, simple
######## structure.

# Remove problematic documents:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData") %>% 
  filter(!is.na(Wars) & !is.na(Currency_crisis_severe))

unique(mydata$type)

# We identify three big groups: programs related, consultations and technical assistance.


group_data <- mydata %>% 
  mutate(family = case_when(type == "request"|type == "review"|type == "cancelation"|
                            type == "modification" ~ "Program related",
                           type == "technical assistance" ~ "Program related",
                           type == "consultation" ~ "Country reports",
         is.na(type) ~ "Country reports"))

unique(group_data$family)

group_data %>% 
  group_by(family) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(family = factor(family, levels = c("Program related","Country reports"))) %>%
  ggplot(aes(family, n, fill = family)) +
  geom_col(width = .2) +
  geom_text(aes(label=n), vjust=0.5, size=3.5)+
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
  geom_text(aes(label=n), vjust= 0.5, size=3)+
  facet_wrap(~ family, ncol = 1, scales = "free_y") +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "none") 

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/corpus_detail2.png")

  

  
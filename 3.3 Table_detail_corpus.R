######## Description: the script generates a histogram with the division of the 
######## corpus into two big families, country reports and programs. A second graph
######## shows for each family the granular ranking of different types of documents.
######## To add: table with short explanation each type of document, whether dismissed or not, simple
######## structure.

# Remove problematic documents:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData") %>% 
  filter(!is.na(Wars) & !is.na(Currency_crisis_severe))

unique(mydata$type)

# We identify two big groups: country reports and program related documents.


group_data <- mydata %>% 
  mutate(family = case_when(type == "request"|type == "review"|type == "cancelation"|
                            type == "modification" ~ "Program Related",
                           type == "technical assistance" ~ "Program Related",
                           type == "consultation" ~ "Country Reports",
         is.na(type) ~ "Country Reports"))



names(mydata)

total=mydata %>% ungroup() %>% dplyr::select(iso3c,period) %>% mutate(year=year(period)) %>% filter(year>=1945) %>%
  group_by(year)%>%
  summarize(n=n()) %>%
  summarize(first=first(year),
            last=last(year)) %>%
  mutate(type_doc="Total") %>% dplyr::select(type_doc,everything())

span_date=mydata %>% ungroup() %>% dplyr::select(iso3c,period,type_doc) %>% mutate(year=year(period)) %>% filter(year>=1945) %>%
  group_by(year,type_doc)%>%
  summarize(n=n()) %>% group_by(type_doc) %>%
  summarize(first=first(year),
         last=last(year)) %>%
  rbind(total)

# Detail corpus graph: ----

# Total country reports and program related docs:

total <- group_data %>% ungroup() %>% 
  dplyr::select(iso3c,period,family) %>% mutate(year=year(period)) %>% filter(year>=1945) %>%
  group_by(family) %>%
 summarize(n=n(),
           first=first(year),
           last=last(year))%>%
  mutate(type_doc = "Total") %>% 
  ungroup() %>% dplyr::select(family,type_doc,n,first,last)

# Granular ranking:

group_data %>% 
  group_by(family, type_doc) %>%
  count() %>% 
  ungroup() %>%
  left_join(span_date,by=c("type_doc")) %>%
  mutate(type_doc=stringr::str_to_title(type_doc),
         type_doc=ifelse(type_doc=="Request For Technical Assistance","Request For T. Assist",type_doc),
         type_doc=ifelse(type_doc=="Cancelation And Request","Canc. & Request",type_doc),
         type_doc=ifelse(type_doc=="Article Iv","Article IV",type_doc),
         type_doc=ifelse(type_doc=="Article Viii","Article VIII",type_doc),
         type_doc=ifelse(type_doc=="Article Xiv","Article XIV",type_doc)) %>%
  rbind(total) %>% 
  mutate(type_doc = fct_reorder(type_doc, n)) %>%
  ggplot(aes(type_doc, n, fill = family)) +
  geom_col(width = .3) +
  geom_text(aes(label=paste0(n," (",first," - ", last,")")), vjust= 0.5,  hjust = -0.04, size=3.5)+
  facet_wrap(~ family, ncol = 1, scales = "free_y") +
  theme_bw() +
  lims(y=c(0,20000))+
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=13),
        strip.text = element_text(size=13,color="black"),
        strip.background = element_blank(),
        axis.text.x = element_text(size =14),
        axis.text.y = element_text(size=9))
        
ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/corpus_detail.png",
       dpi = "retina")

  

  
######## Description: the script generates 
######## 1) a histogram with the division of the corpus into two big families, country reports and programs, and granular ranking all in the same facet.
######## 2) number of documents for each year, family and type doc

# Remove problematic documents:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData") %>% 
  filter(!is.na(Wars) & !is.na(Currency_crisis_severe))

unique(mydata$type)

# We identify two big groups: country reports and program related documents.
# Set new variable and change names for type docs label.


group_data <- mydata %>% 
  mutate(family = case_when(type == "request"|type == "review"|type == "cancelation"|
                            type == "modification" ~ "Program related",
                           type == "technical assistance" ~ "Program related",
                           type == "consultation" ~ "Country reports",
         is.na(type) ~ "Country reports")) %>% 
  mutate(type_doc=stringr::str_to_title(type_doc),
         type_doc=ifelse(type_doc=="Request For Technical Assistance","Request For T. Assist",type_doc),
         type_doc=ifelse(type_doc=="Cancelation And Request","Canc. & Request",type_doc),
         type_doc=ifelse(type_doc=="Article Iv","Article IV",type_doc),
         type_doc=ifelse(type_doc=="Article Viii","Article VIII",type_doc),
         type_doc=ifelse(type_doc=="Article Xiv","Article XIV",type_doc))


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
  geom_text(aes(label =n), vjust= 0.5,  hjust = -0.04, size=3.5)+
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


# Evolution graphs: -----


# Parameters (family, colors and docs to exclude from facet):
  
family <- c("Country reports","Program related")
color <- c("#F8766D","#00BFC4")

group_data <- group_data %>% 
  filter(type_doc != "Canc. & Request" & 
        type_doc !="Cancelation" & 
        type_doc != "Modification"&
        type_doc !="Performance Criteria"&
        type_doc != "Purchase Transac"&
        type_doc !="Use Fund"&
        type_doc !="Waiver")

# Plot:

evolution_graphs <- family %>% 
map2(color,function(x,y){
  group_data %>% 
  group_by(family,type_doc, year) %>% 
  count() %>%
  filter(family == x) %>%  
  ggplot(aes(year,n)) +
  geom_col(fill = y, col = "white") +
  facet_wrap(~ type_doc) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(size=14), axis.text.x = element_text(size =14,angle=90)) +
  theme(strip.text = element_text(face="bold", size=14)) +
  labs(fill = "")}
)

names(evolution_graphs) <- family

evolution_graphs %>% 
map2(names(evolution_graphs),~ ggsave(paste0( "../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/corpus_evolution_",.y,".png"),
      plot = .x,
      dpi = "retina")
)


#typology of categories: exogeneous/endogeneous and Economic/non economic shocks

stargazer::stargazer(title="Typology of indexes"
                     , lexicon_typology()
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=T
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out="../Betin_Collodel/2. Text mining IMF_data/output/Typology indexes/Typology_indexes.tex"
)


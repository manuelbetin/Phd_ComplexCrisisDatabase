# Description: construction table with documents by country and distribution graph 

# Distribution: -----

rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  group_by(ISO3_Code) %>% 
  count() %>% 
  ggplot(aes(n)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=12,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  theme_bw() +
  ylab("") +
  xlab("")


ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/ctry_detail.png")



# Table ----

# First and last document by country


first_year <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  group_by(ISO3_Code) %>% 
  filter(year == min(year)) %>% 
  filter(!duplicated(year)) %>% 
  select(ISO3_Code, year)

rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  group_by(ISO3_Code) %>% 
  count() %>% 
  merge(first_year) %>% 
  mutate(`Country name` = countrycode(ISO3_Code, "iso3c","country.name")) %>% 
  select(ISO3_Code, `Country name`, year, n) %>% 
  rename(ISO3 = ISO3_Code,`First document` = year, `N. of documents` = n) %>% 
  stargazer(summary = F, out = "../Betin_Collodel/2. Text mining IMF_data/output/tables/Corpus/ctry_detail.tex")

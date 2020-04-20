######### Description: the script generates a table with the number of documents by country
######### and the year of first document, the distribution graph and corresponding map.
######### All the output saved in Dropbox folder.

# Distribution: -----

documents_ctry <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  group_by(ISO3_Code) %>% 
  count() 

  documents_ctry %>% 
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


# Globe map -----


world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  rename(ISO3_Code = wb_a3) %>% 
  merge(documents_ctry, by = "ISO3_Code")

ggplot(data = world) +
  geom_sf(aes(fill = n),col = "black") +
  scale_fill_gradient(low='white', high='#F8766D',name = "Number of documents") +
  theme_bw()

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/ctry_detail.png")

 



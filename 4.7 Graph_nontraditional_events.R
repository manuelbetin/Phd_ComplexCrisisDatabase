######## Description: the script generates graph for the time series of some less standard events

# Average data over year:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

# Notable examples of social crises -------

# Labor bill strikes and gilet jaunes movement:

mydata %>% 
  filter(ISO3_Code == "FRA") %>% 
  ggplot(aes(year, Social_crisis, group =1, col = ISO3_Code)) +
  geom_line() +
  geom_point() +
  geom_rect(aes(xmin = 2016, xmax = 2019, ymin = -Inf, ymax = Inf),fill = "red", alpha = 0.02) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")

# Hong Kong protests:

mydata %>% 
  filter(ISO3_Code == "CHN") %>% 
  ggplot(aes(year, Social_crisis, group =1, col = ISO3_Code)) +
  geom_line() +
  geom_point() +
  geom_rect(aes(xmin = 2018, xmax = 2019, ymin = -Inf, ymax = Inf),fill = "red", alpha = 0.02) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")



# Notabe examples of migration crises -----

# Colombian refugee crisis from Venezuela

mydata %>% 
  filter(ISO3_Code == "COL" & year >= 1980) %>% 
  ggplot(aes(year, Migration, group =1, col = ISO3_Code)) +
  geom_line() +
  geom_point() +
  geom_rect(aes(xmin = 2018, xmax = 2019, ymin = -Inf, ymax = Inf),fill = "red", alpha = 0.02) +
  theme_bw() + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")

# Lebanon refugee crisis fronm Syria

mydata %>% 
  filter(ISO3_Code == "LBN" & year >= 1980) %>% 
  ggplot(aes(year, Migration, group =1, col = ISO3_Code)) +
  geom_line() +
  geom_point() +
  geom_rect(aes(xmin = 2012, xmax = 2019, ymin = -Inf, ymax = Inf),fill = "red", alpha = 0.02) +
  theme_bw() + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")



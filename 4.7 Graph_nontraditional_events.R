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
  ggplot(aes(year, Social_crisis, group =1)) +
  geom_line(col = "#00BFC4") +
  geom_point(col = "#00BFC4") +
  geom_vline(xintercept = mydata$year[which(mydata$Social_crisis >0 & mydata$ISO3_Code == "FRA")],col = "grey10", linetype = "dotted") +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = c(1950,1960, 1970, 1980, 1990, 2000,2010, 2020)) + 
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none")

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Non_traditional_events/FRA_gilet_jaune.png")

# Hong Kong protests:

mydata %>% 
  filter(ISO3_Code == "CHN") %>% 
  ggplot(aes(year, Social_crisis, group =1)) +
  geom_line(col = "#00BFC4") +
  geom_point(col = "#00BFC4") +
  geom_vline(xintercept = mydata$year[which(mydata$Social_crisis >0 & mydata$ISO3_Code == "CHN")],col = "grey10", linetype = "dotted") +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = c(1950,1960, 1970, 1980, 1990, 2000,2010, 2020)) + 
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none")

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Non_traditional_events/CHN_hong_kong_protests.png")



# Notabe examples of migration crises -----

# Colombian refugee crisis from Venezuela

mydata %>% 
  filter(ISO3_Code == "COL" & year >= 1980) %>% 
  ggplot(aes(year, Migration, group =1)) +
  geom_line(col = "#7CAE00") +
  geom_point(col = "#7CAE00") +
  geom_vline(xintercept = mydata$year[which(mydata$Migration >0 & mydata$ISO3_Code == "COL" & mydata$year > 1980)],col = "grey10", linetype = "dotted") +
  theme_bw() + 
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = c(1980, 1990, 2000,2010, 2020)) + 
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none")

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Non_traditional_events/COL_migration.png")

# Lebanon refugee crisis fronm Syria

mydata %>% 
  filter(ISO3_Code == "LBN" & year >= 1980) %>% 
  ggplot(aes(year, Migration, group =1)) +
  geom_line(col = "#7CAE00") +
  geom_point(col = "#7CAE00") +
  geom_vline(xintercept = mydata$year[which(mydata$Migration >0 & mydata$ISO3_Code == "LBN" & mydata$year > 1980)],col = "grey10", linetype = "dotted") +
  theme_bw() + 
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = c(1980, 1990, 2000,2010, 2020)) + 
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none")

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Non_traditional_events/LBN_migration.png")


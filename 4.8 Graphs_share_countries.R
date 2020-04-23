######## Description: the script generates graph for the probability part 
######## Graph 1: share of countries with currency crisis, Graph 2: correlation with inflation crisis
######## To do: automate correlation in graph 2


# Average by year:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

# Binary indicator:

get_prob <- function(x){
  ifelse(x > 0,1,0)
}

# Graph 1
# Share of country experiencing currency crisis, 1950-2020: (5-years moving average)

mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1950) %>% 
  summarise(share_cc = mean(Currency_crisis_severe, na.rm = T)) %>%
  mutate(share_cc_ma = zoo::rollmean(share_cc, 5, align = "center", fill = NA)) %>% 
  ggplot(aes(year)) +
  geom_col(aes(y = share_cc), fill = "grey50", alpha = 0.4) +
  geom_line(aes(y=share_cc_ma, col = "share"), size = 1.5) +
  theme_bw() +
  ylab("Share of countries (%)") +
  xlab("") +
  scale_x_continuous(breaks = c(1950,1960, 1970, 1980, 1990, 2000,2010, 2020)) + #set x ticks
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none")

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/Share_Currency.png")

# Graph 2
# Share of country experiencing currency crisis and inflation crisis, 1950-2020: (5-years moving average)
# Correlation

mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1950) %>% 
  summarise(`Share Currency Crisis` = mean(Currency_crisis_severe, na.rm = T), `Share Inflation Crisis` = mean(Inflation_crisis, na.rm = T)) %>%
  mutate(`Share Currency Crisis` = zoo::rollmean(`Share Currency Crisis`, 5, align = "center", fill = NA),`Share Inflation Crisis` = zoo::rollmean(`Share Inflation Crisis`, 5, align = "center", fill = NA)) %>%
  gather("type", "share", `Share Currency Crisis`:`Share Inflation Crisis`) %>% 
  ggplot(aes(year, share, col = type)) +
  geom_line(size = 1.5) +
  theme_bw() +
  xlab("") +
  ylab("Share of countries (%)") +
  annotate(geom="text", x=1965, y=0.6, label="italic(Cor): .89", parse = T, size= 6)+
  scale_x_continuous(breaks = c(1950,1960, 1970, 1980, 1990, 2000,2010, 2020)) + #set x ticks
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none", legend.title = element_blank()) 


ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/Share_Currency_Inflation.png")

# Graph 3 
# Share of countries experiencing Migration crisis, 1950-2000: (5-years moving average)

mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1946) %>% 
  summarise(share_migration = mean(Migration, na.rm = T)) %>%
  mutate(share_migration_ma = zoo::rollmean(share_migration, 5, align = "center", fill = NA)) %>% 
  ggplot(aes(year)) +
  geom_col(aes(y= share_migration),fill = "grey50", alpha = 0.4) +
  geom_line(aes(y= share_migration_ma),col = "#7CAE00", size = 1.5) +
  theme_bw() +
  ylab("Share of countries (%)") +
  xlab("") +
  scale_x_continuous(breaks = c(1950,1960, 1970, 1980, 1990, 2000,2010, 2020)) + #set x ticks
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none")

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/Share_Migration.png")


# Graph 4 
# Share of countries experiencing Social crisis, 1950-2000: (5-years moving average)


mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1946) %>% 
  summarise(share_social = mean(Social_crisis, na.rm = T)) %>%
  mutate(share_social_ma = zoo::rollmean(share_social, 5, align = "center", fill = NA)) %>% 
  ggplot(aes(year)) +
  geom_col(aes(y = share_social),fill ="grey50", alpha = 0.4) +
  geom_line(aes(y = share_social_ma),col ="#00BFC4", size = 1.5) +
  theme_bw() +
  ylab("Share of countries (%)") +
  xlab("") +
  scale_x_continuous(breaks = c(1950,1960, 1970, 1980, 1990, 2000,2010, 2020)) + #set x ticks
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text=element_text(size=19), axis.title = element_text(size= 22)) +
  theme(legend.position = "none")

ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Probability/Share_Social.png")

  



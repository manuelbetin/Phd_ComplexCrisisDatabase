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

share_cc_graph <- mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1946) %>% 
  summarise(share_cc = mean(Currency_crisis_severe, na.rm = T)) %>%
  mutate(share_cc_ma = zoo::rollmean(share_cc, 5, align = "center", fill = NA)) %>% 
  ggplot(aes(year)) +
  geom_col(aes(y = share_cc), fill = "grey40") +
  geom_line(aes(y=share_cc_ma, col = "share")) +
  theme_bw() +
  ylab("Share of countries (%)") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")

# Graph 2
# Share of country experiencing currency crisis and inflation crisis, 1950-2020: (5-years moving average)
# Correlation

share_cc_infl_graph <- mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1946) %>% 
  summarise(`Share Currency Crisis` = mean(Currency_crisis_severe, na.rm = T), `Share Inflation Crisis` = mean(Inflation_crisis, na.rm = T)) %>%
  mutate(`Share Currency Crisis` = zoo::rollmean(`Share Currency Crisis`, 5, align = "center", fill = NA),`Share Inflation Crisis` = zoo::rollmean(`Share Inflation Crisis`, 5, align = "center", fill = NA)) %>%
  gather("type", "share", `Share Currency Crisis`:`Share Inflation Crisis`) %>% 
  ggplot(aes(year, share, col = type)) +
  geom_line() +
  theme_bw() +
  xlab("") +
  ylab("") +
  annotate(geom="text", x=1965, y=0.6, label="italic(Cor): .89", parse = T) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none", legend.title = element_blank()) 


grid.arrange(share_cc_graph, share_cc_infl_graph, ncol = 2)

# Graph 3 
# Share of countries experiencing Migration crisis, 1950-2000: (5-years moving average)

share_migration_graph <- mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1946) %>% 
  summarise(share_migration = mean(Migration, na.rm = T)) %>%
  mutate(share_migration_ma = zoo::rollmean(share_migration, 5, align = "center", fill = NA)) %>% 
  ggplot(aes(year)) +
  geom_col(aes(y= share_migration),col = "grey60") +
  geom_line(aes(y= share_migration_ma),col = "#7CAE00") +
  theme_bw() +
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")


# Graph 4 
# Share of countries experiencing Social crisis, 1950-2000: (5-years moving average)


share_social_graph <- mydata %>% 
  mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year >= 1946) %>% 
  summarise(share_social = mean(Social_crisis, na.rm = T)) %>%
  mutate(share_social_ma = zoo::rollmean(share_social, 5, align = "center", fill = NA)) %>% 
  ggplot(aes(year, col="share")) +
  geom_col(aes(y = share_social),col ="grey60") +
  geom_line(aes(y = share_social_ma),col ="#00BFC4") +
  theme_bw() +
  ylab("Share of countries (%)") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")

grid.arrange(share_social_graph, share_migration_graph, ncol = 2)



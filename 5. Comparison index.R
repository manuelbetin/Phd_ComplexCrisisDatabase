# Comparison tf-idf indexes with variables such as nominal exchange rate, reserves
# and other standard databases.

library(purrr)
library(rio)
library(rlang)
library(dplyr)
library(sloop)
library(stringr)
library(tidyverse)
library(rio)


# Upload all the indexes into a list ----


path_project = "/Users/Umberto/Dropbox/Early warning model of currency crisis/" # Path to project folder, with codes directory and data.
path_tf_idf = "temp/tf"

# Upload: 

tf_idf <- list.files(paste(getwd(),"/",path_tf_idf, sep = "")) %>% 
  map(~ paste(getwd(),"/",path_tf_idf,"/",.x, sep = "")) %>%
  map(import) %>% 
  map(~ .x %>% mutate(country = str_extract(file, "[A-Z]{3}"),year = str_extract(file, "\\d{4}"))) %>%    # create a year and country identifier.
  map(~ .x %>% group_by(country,year)) %>% # average the indexes  per year if we created an index of both currency crises and severe currency crises, otherwise return normal dataframe.
  map(~  if("Currency_crisis" %in% names(.x) & "Currency_crisis_severe" %in% names(.x)) {
        .x %>% summarise(cc_severe = mean(Currency_crisis_severe), cc = mean(Currency_crisis))
  }
      else(.x)
  )

# Assign name of country to each dataframe:

names(tf_idf) <- str_extract(list.files(paste0(getwd(),"/",path_tf_idf,sep = "")),"[A-Z]{3}")

# Upload comparison variables ----

# Nominal exchange rates and reserves:

comparison_variables <- import(paste0(path_project,"Betin_Collodel/2. Text Mining IMF_Data/datasets/comparison index/comparison_variables.Rdata")) %>% 
  mutate(ner_avg_growth = ((ner_avg - dplyr::lag(ner_avg, 1))/dplyr::lag(ner_avg,1))*100)  # creaation growth rates for the nominal exchange rate.

# Reinhart & Rogoff crisis database:

RR_database <- import(paste0(path_project,"Betin_Collodel/2. Text Mining IMF_Data/datasets/comparison index/Data_crisis_q.Rdata")) %>% 
  select(ISO3_Code, year, quarter, CC.RR_first) %>%   # The variable CC.RR_first is set equal to 1 only in the first quarter of the year if country experienced currency crisis.
  filter(quarter == 1) %>% 
  select(-quarter) %>% 
  rename(country = ISO3_Code)

# Plot for all countries into the list the two indexes (severe/non-severe) ----


tf_idf_plots <- tf_idf %>%
  map(~ if("cc" %in% names(.x) & "cc_severe" %in% names(.x)) { # For the moment, we consider only countries for which a severe currency crisis index was created with text mining. 
  .x %>%
        ggplot(aes(year, group = 1)) +
        geom_line(aes(y = cc_severe, col = "Severe Index")) +
        geom_line(aes(y = cc, col = "Normal Index")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  else {NULL}
)


# One-by-one inspection ----
# Thailand.


tf_idf_plots[["THA"]]

# It is quite interesting that the normal index identifies a crisis in 1970, but the severe one not.
# Go back to the document to identify the keywords.
# We identify the documents responsible for this. 
# The only word of the list contained in the document "THA_1970-05-25_Eco developments" is "cumulative depreciation".
# Nevertheless, the word indicates a cumulative depr. of 0.99 %. For sure, not currency crises.


# Comparison with standard measures Thailand: 

# Get the series for Thailand

example_tha <- tf_idf[["THA"]] 

# Compare to the exchange rate movements in sd:

example_tha %>%
  merge(comparison_variables, by = c("country","year")) %>% 
  ggplot(aes(year, group = 1)) +
  geom_line(aes(y = scale(ner_avg_growth), col = "NER Growth")) +
  geom_line(aes(y = scale(cc), col = "Normal Index")) +
  geom_line(aes(y = scale(cc_severe), col = "Severe Index")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("sd")

# Nominal exchange rate alone to understand magnitude:

comparison_variables %>% 
  filter(country %in% "THA") %>% 
  ggplot(aes(year, ner_avg_growth, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# RR database:

RR_database %>% 
  filter(country %in% "THA") %>% 
  ggplot(aes(year, CC.RR_first)) +
  geom_col()




# Compare to the exchange rate movements for any country ----

plot_comparison <- 
  tf_idf %>% 
  map(~ if("cc" %in% names(.x) & "cc_severe" %in% names(.x)){      # First chunk: if have both the cc and cc severe index, merge with
  intermediate <- merge(.x, comparison_variables)                  # nominal exchange rate and RR database. Otherwise, NULL.
  merge(intermediate, RR_database)
  }
  else{NULL}
  ) %>% 
  map(~ if(!is.null(.x)){                                          # If not NULL, return list: plot the behaviour of nominal exchange rate growth,
    list(plot = .x %>%                                             # normal index and severe index (scaled sd) and nominal exchange rate growth.
      ggplot(aes(year, group = 1)) +
      geom_line(aes(y = scale(cc), col = "Normal Index")) +
      geom_line(aes(y = scale(cc_severe), col = "Severe Index")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("sd") +
      ggtitle(as.character(.x$country)) +
      theme(plot.title = element_text(hjust = 0.5)),
   nominal_exchange_rate = .x %>%
      ggplot(aes(year, ner_avg_growth, group = 1)) +
      geom_line() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("%") +
      ggtitle(as.character(.x$country)) +
      theme(plot.title = element_text(hjust = 0.5)),
   rr_crisis = .x %>% 
     ggplot(aes(year, CC.RR_first, group = 1)) +
     geom_col() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     ylab("%") +
     ggtitle(as.character(.x$country)) +
     theme(plot.title = element_text(hjust = 0.5))
    )
  }
  )




# Individual country comparison ----

# Brazil ----
# In 1964 the key-word "large devaluations" is captured by the severe index.
# In 1971, depreciation of 15% and RR signal crisis: "The practice of small devaluations at frequent, irregular intervals, which was adopted three years ago, has played an essential role i n
# developing Brazilian exports". Good that our index does not capture it.
# In 1973, our normal index captures a crisis, but the severe index misses it. RR identifies it in 1974. The word responsible is "cumulative depr.".
# In any case, no crisis. Part of the Brazilian plan of small and regular depreciations.
# In 1985 index capturing Lebanon article IV.
# In 1998 starts the Brazilian crisis.
# Should pay more attention to the 80's period. Until '77 and afterwards it is fine.



plot_comparison[["BRA"]][[1]]
plot_comparison[["BRA"]][[2]]
plot_comparison[["BRA"]][[3]]



# Korea ----

# Before 1975, all the currency crises identified by RR seem more moves toward market
# determined exchange rate. 1975 is an interesting case: to discuss.
# In 1985 there don't seem to be problem: we detect a "sharp depreciation", that, however, does not refer to US dollar.


plot_comparison[["KOR"]][[1]]
plot_comparison[["KOR"]][[2]]
plot_comparison[["KOR"]][[3]]


# Maleysia ----
# In 1987, the index identifies a crisis: "large depreciation of the US dollar".
# Minutes meeting: taking information of exchange rate crisis in Suriname.
# In 1995, interesting case: appreciation against the US dollar, but large depreciation against all main trading
# partners because of the contagion effect from Mexico. (MYS_1995-09-29_Article_IV). Takes into account more information.
# After the Asian crisis, in 2001, still high why? Taking a "large depreciation" included into the notes to explain
# a model ??-la Krugmann.
# In 2004 some problems: they talk about currency crisis in 1998 and that's why the index spikes. Nevertheless, no crisis.



plot_comparison[["MYS"]][[1]]
plot_comparison[["MYS"]][[2]]
plot_comparison[["MYS"]][[3]]



# Kazakhstan -----
# In 1999, crisis of contagion in Kazakhstan: contagion from the Russian crisis.
# In 2005-2006 the index should stay constant: "sharp depreciation" captured in the notes (KAZ_2005-06-08). Hypothetical scenario:
# "While a sharp depreciation..." Same in 2006.


plot_comparison[["KAZ"]][[1]]
plot_comparison[["KAZ"]][[2]]
plot_comparison[["KAZ"]][[3]]


# Uruguay -----
# Why in 1963 the index is not spiking? "Sharp depreciation" term present into the document.


plot_comparison[["URY"]][[1]]
plot_comparison[["URY"]][[3]]

# Russian Federtion ----

plot_comparison[["RUS"]][[1]]
plot_comparison[["RUS"]][[2]]
plot_comparison[["RUS"]][[3]]

# China ----
# Why the spike in 2006? "despite the sharp depreciation in the U.S. dollar"


plot_comparison[["CHN"]][[1]]
plot_comparison[["CHN"]][[2]]
plot_comparison[["CHN"]][[3]]


# Export plots -----

path_figures_comparison <- paste(path_project, "Betin_Collodel/2. Text Mining IMF_data/output/figures/Comparison", sep = "")

# Create new directory if it does not exist already:

if(!dir.exists(path_figures_comparison)){
dir.create(path_figures_comparison)
}

# Only plots severe vs. non-severe index:

plots <- plot_comparison %>% 
  map(~ .x[["plot"]])

1:length(plots) %>% 
  walk(function(i) {
    if(!is.null(plots[[i]])){
    ggsave(filename = paste(path_figures_comparison,"/","comparison_",names(plots)[i],".png",sep = ""), plot = plots[[i]])}
  })





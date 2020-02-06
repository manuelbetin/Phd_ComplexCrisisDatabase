# Comparison tf-idf indexes with variables such as nominal exchange rate, reserves
# and other standard databases.

library(purrr)
library(rio)
library(rlang)
library(dplyr)
library(sloop)
library(stringr)
library(tidyverse)


# Upload all the indexes into a list ----

path_project = "/Users/Umberto/Dropbox/Early warning model of currency crisis/" # Path to project folder, with codes directory and data.

path_old_tf_idf = "Betin_Collodel/temp_umberto/tf" # Downloaded some old tf-idf with my keywords, to not overwrite Manu's tf-idf keep them in a different folder for the moment.

# Upload: 

tf_idf <- list.files(paste0(path_project,"/",path_old_tf_idf, sep = "")) %>% 
  map(~ paste(path_project,"/",path_old_tf_idf,"/",.x, sep = "")) %>% 
  map(import) %>% 
  map(~ .x %>% mutate(country = str_extract(file, "[A-Z]{3}"),year = str_extract(file, "\\d{4}"))) %>%    # create a year and country identifier.
  map(~ .x %>% group_by(country,year)) %>% # average the indexes  per year if we created an index of both currency crises and severe currency crises, otherwise return normal dataframe.
  map(~  if("Currency_crisis" %in% names(.x) & "Currency_crisis_severe" %in% names(.x)) {
        .x %>% summarise(cc_severe = mean(Currency_crisis_severe), cc = mean(Currency_crisis))
  }
      else(.x)
  )

# Assign name of country to each dataframe:

names(tf_idf) <- str_extract(list.files(paste0(path_project,"/",path_old_tf_idf)),"[A-Z]{3}")


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

example_df %>%
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
  intermediate <- merge(.x, comparison_variables)                  # nominal exchange rate and RR database. Otherwise, NULL
  merge(intermediate, RR_database)
  }
  else{NULL}
  ) %>% 
  map(~ if(!is.null(.x)){                                          # If not NULL, return list: plot the behaviour of nominal exchange rate growth,
    list(plot = .x %>%                                             # normal index and severe index (scaled sd) and nominal exchange rate growth.
      ggplot(aes(year, group = 1)) +
      geom_line(aes(y = scale(ner_avg_growth), col = "NER Growth")) +
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


plot_comparison[["KOR"]][[1]]

plot_comparison[["KOR"]][[2]]

plot_comparison[["KOR"]][[3]]





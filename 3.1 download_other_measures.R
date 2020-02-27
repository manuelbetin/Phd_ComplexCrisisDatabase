# Packages to use:

library(devtools)
devtools::install_github("umbertocollodel/umbcustom")
library(umbcustom)
library(tidyverse)
library(purrr)
library(countrycode)

# We compare the tf-idf index we generated with the level of the nominal exchange rate,
# the level of international reserves/gdp (growth-rate) and some standard databases of currency crises.


# Download the nominal exchange rate and international reserves from the IFS ----

# Create a list with series name:

serie_names <- c("ENDE_XDC_USD_RATE","ENDA_XDC_USD_RATE","RAXG_USD","NGDP_XDC")

# For each serie name, download dataframe:

comparison_data <- serie_names %>%
  map(~ download_imf("IFS","1950","2019","A", .x)) %>% 
  reduce(merge, by = c("@REF_AREA","@TIME_PERIOD"),all = TRUE) %>%
  setNames(c("country","year","ner_eop","ner_avg","reserves","ngdp")) %>% 
  mutate(country = countrycode(country,"iso2c","iso3c")) %>%   # convert from iso2 to iso3 as in tf-idf
  filter(!is.na(country))   # and remove NAs (aggregate areas)

# Create reserves growth and reserves over gdp:

comparison_variables <- comparison_data %>% 
  mutate(ngdp_usd  = ngdp/ner_avg) %>% 
  mutate(resgdp = reserves/ngdp_usd*100) %>% 
  group_by(country) %>% 
  mutate(resgrowth = (reserves - dplyr::lag(reserves, 1))/dplyr::lag(reserves, 1)) %>% 
  select(-reserves, -ngdp, -ngdp_usd)

# Create a new directory with comparison Rdata cleaned and save them:

comparison_path <- "~/Umberto/Dropbox/Betin_Collodel/2. Text Mining IMF_data/datasets/comparison index"

if(!dir.exists(comparison_path)){
  dir.create(comparison_path)
}

save(comparison_variables, file = paste(comparison_path,"/comparison_variables.Rdata", sep = ""))



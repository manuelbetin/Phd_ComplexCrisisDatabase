
source("1. Consolidate_urls.R")

source("2. Run_text_mining.R")

source("3. Clean_database.R")

source("3.2. check_validity_extractions.R")

source("4. Analysis.R")

a=mydata %>% filter(iso3c=="GRC")


b=LoI_tf_idf %>% filter(ISO3_Code=="BRA")
head(b)
ggplot()+
  geom_line(data=b,aes(x=Period,y=Currency_crisis))+
  geom_point(data=b,aes(x=Period,y=Currency_crisis))

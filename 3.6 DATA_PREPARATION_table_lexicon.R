######## Description: the script generates 
######## 1) a table with a summary of the lexicon used.

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis',
         'Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

summary_lexicon <- lexicon()[-length(lexicon())] %>% 
    map(~ data.frame(`Total Number` = length(.x), Keywords = .x)) %>% 
    map(~ .x %>% slice(1:4)) %>%
    map(~ .x %>% mutate(Keywords = paste(Keywords, collapse = ", "))) %>% 
    map(~ .x %>% mutate(Keywords = paste0(Keywords," ..."))) %>%
    map(~ .x %>% slice(1)) %>%  
    bind_rows(.id = "Category") %>% 
    arrange(-`Total.Number`) %>%
    rename(`Total number` = `Total.Number`) %>% 
    filter(Category %in% shocks) %>% 
    mutate(Category = str_remove(Category, "_crisis")) %>% 
    mutate(Category = case_when(Category == "Balance_payment" ~ "BoP", Category == "Sovereign_default" ~ "Sovereign",
                                Category == "World_outcomes" ~ "World", Category == "Currency_severe" ~ "Currency",
                                Category == "Natural_disaster" ~ "Nat. disaster", Category == "Severe_recession" ~ " Sev. Recession",
                                Category == "Soft_recession" ~ "Soft recession", TRUE ~ Category))

stargazer::stargazer(summary_lexicon, summary = F, out = "../Betin_Collodel/2. Text mining IMF_data/output/tables/Lexicon/summary_lexicon.tex")                                

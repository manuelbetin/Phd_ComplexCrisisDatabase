#                          Text Mining for Economic Crisis

***
***

## Description

Text mining on pdf documents. 

### Codes

-**0.main.R** source all intermediary scripts
-**
- **1.1. consolidate_urls.R**: take the full list of urls and subsample only the relevant documents
- **1. Clean_urls_report.rmd:** once the list of urls is ready use this script to clean the urls and generate report
- **2. Run_Text_mining.r** use the dataframe of urls to download pdfs, generate the corpus,and run the text mining on the documents
- **3. Clean_database.r** use the dataframe of urls to download pdfs, generate the corpus,and run the text mining on the documents
- **3.1 download_other_measures.R**
- **3.2 check_validity_extractions.R**
- **4. Analysis.R**
- **4.1 Analysis_tf_idf_decades.R**
- **4.2 Analysis_timing_shocks.R**
- **4.3 Analysis_ex_timeseries.R**
- **4.4 Analysis_composite_crisis_index.R**
- **4.5 Analysis_clusters_shocks.R**
- **4.6 Analysis_timing_tf.R**
- **5. Comparison_index.R**
- **5.1.1 Tab_two.R**
- **5.1.1 Tab_three.R**
- **5.1.1 Tab_four.R**
- **5.2 Comparison_app.R**
- **5.3 Comparison_output_losses.R**
- **6. Crisis_network.R**
- **6.1 Crisis_network_complexity.R**


## Author

- Manuel Betin
- Umberto Collodel

## Language

- R 

## Dependencies

### Libraries

- "manuelbetin/SetUpProject" available from github (private access)
- "manuelbetin/TextMiningCrisis" available from github (private access)
- pdftools 
- xml2
- rvest
- tidytext
- tidyr
- stringr
- stringi
- tidytext
- dplyr
- tidyr
- plotly
- ggplot2
- rio
- tictoc
- lubridate
- filesstring
- wbstats


## Structure and workflow of code

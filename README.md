#                          Text Mining for Economic Crisis

***
***

## Description

Text mining on pdf documents. 

### Codes

- **1.1. consolidate_urls.R**: take the full list of urls and subsample only the relevant documents
- **1. Clean_urls_report.rmd:** once the list of urls is ready use this script to clean the urls and generate report
- **2. Run_Text_mining.r** use the dataframe of urls to download pdfs, generate the corpus,and run the text mining on the
documents

## Author

- Manuel Betin

## Language

- R 

## Dependencies

### Libraries

"manuelbetin/SetUpProject" available from github (private access)
"manuelbetin/TextMiningCrisis" available from github (private access)
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
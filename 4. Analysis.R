##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = here::here()
setwd(current_path)
root_path=current_path
##install common packages
library("devtools") #make sure you have the library
#install_github("manuelbetin/SetUpProject",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
#install_github("manuelbetin/TextMiningCrisis",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
install_github("manuelbetin/PICindex",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")

packages <- c("dplyr"
              ,"ggplot2"
              ,"plotly"
              ,"pdftools"
              ,"lubridate"
              ,'tictoc'
              ,"rio"
              ,"tidytext"
              ,"stringr"
              ,"stringi"
              ,"tidyr"
              ,"network"
              ,"GGally"
              , "TextMiningCrisis"
              , "SetUpProject"
              , "PICindex",
              "gridExtra"
              )

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## INSTRUCTIONS ##########

# Description:
# The script takes as input the term frequency matrix and the dataset created by 3.3. Clean_database.r
# to perform exploratory analysis and grafical representations by sourcing differenct files
# 4.1. Analysis_tf_idf_decades.r:
#     Creates radarchart of tf-idf for each decade for shocks, structure and reforms
# 4.2; Analysis_timing_shocks.r
#     Plots the occurence of shocks from request to midterm review and final review to observe how shocks
#     evolve during the whole period of the debt distress
# 4.3; Analysis_ex_timeseries.r
#     Profide an example of time series for different shocks for a selected country
# 4.4. Analysis_exogeneity_shocks.r
#     Look at degree of exogeneity of shocks by looking at the correlationg between the shocks
# 4.5. Analysis_clusters_shocks.r
#     Create a cluster analysis with kmean methods
# 4.6. Analysis_timing_tf.r
#     Observe term frequency barplot segmented by review to observe the evolution of priority during the
#     duration of the distress
#***************************************************************************************####

output=list()
output[["Session_info"]]=sessionInfo()

# Import data ####

#raw term frequency with only name of files and the tf variables
LoI_tf_idf=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")

#Complete information combining tf, url metadata and quantititve measures
mydata=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData")

#typology of categories: exogeneous/endogeneous and Economic/non economic shocks

stargazer::stargazer(title="Typology of indexes"
          , typology_categories()
          , type="latex"
          , digits=2
          , no.space=T
          , align=T
          , summary=F
          , rownames=T
          , table.placement = "H"
          , column.sep.width="3pt"
          , font.size = "footnotesize"
          , out="../Betin_Collodel/2. Text mining IMF_data/output/Typology indexes/Typology_indexes.tex"
          )

# evolution of tf-idf across decades and radar chart ####


source("4.1. Analysis_tf_idf_decades.r")

# Timing and sequence of the crisis ####

source("4.2. Analysis_timing_shocks.r")

# Plot time series of evolution ------

ctry=c("IND")
#c("MEX","ARG","USA","URY","IDN","THA","DEU","GRC")

#,"ARG","THA","MEX","USA","TUR","FRA","BRA","IDN")
source("4.3. Analysis_ex_timeseries.r")

# Compute the composite crisis index ####

#list of shocks to consider
shocks=c('Natural_disaster','Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',
         'Severe_recession','Sovereign_default',"Currency_crisis_severe","Wars","Social_crisis")

source("4.4. Analysis_composite_crisis_index.r")

# clusters shocks #########

source("4.5. Analysis_clusters_shocks.r")

# Average term frequency ####

#source("4.6. Analysis_timing_tf.r")

# Transformation functions #####

source("4.7. Analysis_transformation_function.R")


# save output ####

save(output,file="../Betin_Collodel/2. Text mining IMF_data/output/Analysis/Output_Analysis.RData")

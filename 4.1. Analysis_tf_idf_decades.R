
buckets=list(
  bucket1=c(1960,1970),
  bucket2=c(1970,1980),
  bucket3=c(1960,1980),
  bucket4=c(1980,1995),
  bucket5=c(1995,2005),
  bucket6=c(2005,2013))

# tf_by_group=function(table_N_occurence,weight_method="brut_frequency",mygroup=NULL){
#   if(is.null(mygroup)){
#     #dt_weights_years=tf_idf(table_N_occurence,weight_method) %>% ungroup() #dplyr::summarize_if(is.numeric,mean)
#     avoid_colums=c(names(table_N_occurence %>% dplyr::select_if(is.character)),names(table_N_occurence %>% dplyr::select_if(is.Date)))
#     dt_weights_years=table_N_occurence %>% tidyr::gather("Crisis",value="word_weight",-c(avoid_colums))
#     dt_weights_years=dt_weights_years[-1,]
#     dt_weights_years=dt_weights_years %>% dplyr::mutate(word_weight=as.numeric(word_weight))
#     dt_weights_years
#   }else{  
#     avoid_colums=c(names(table_N_occurence %>% dplyr::select_if(is.character)),names(table_N_occurence %>% dplyr::select_if(is.Date)))
#     
#     dt_weights_years=table_N_occurence %>% tidyr::gather("Crisis",value="word_weight",-c(avoid_colums))
#     dt_weights_years=dt_weights_years[-1,]
#     dt_weights_years=dt_weights_years %>% group_by(get(mygroup)) %>% dplyr::mutate(word_weight=as.numeric(word_weight))
#     
#   }
# }

output[["tf-idf_fig_year_radars"]]=lapply(buckets,function(x){
  start=x[1]
  end=x[2]
  shocks=list()
  reforms=list()
  structure=list()
  dt_year_radar=tf_by_group(LoI_tf_idf %>% filter(file %in% (mydata %>% filter(type_doc=="request"))$file))
  dt_year_radar=dt_year_radar %>% group_by(Crisis) %>% mutate(year=year(Period)) %>% filter(year>=start & year<end) %>% summarize(word_weight=mean(word_weight),
                                                                                                                                  Period=paste0(min(year),"-",max(year)))
  shocks[["request"]]=country_radar_fig(dt_year_radar)
  reforms[["request"]]=TextMiningCrisis::radar_reforms_fig(dt_year_radar)
  structure[["request"]]=TextMiningCrisis::radar_structure_fig(dt_year_radar)
  
  dt_year_radar=tf_by_group(LoI_tf_idf %>% filter(file %in% (mydata %>% filter(type_doc=="review"))$file))
  dt_year_radar=dt_year_radar %>% group_by(Crisis)%>% mutate(year=year(Period)) %>% filter(year>=start & year<end) %>% summarize(word_weight=mean(word_weight),
                                                                                                                                 Period=paste0(min(year),"-",max(year)))
  shocks[["review"]]=country_radar_fig(dt_year_radar)
  reforms[["review"]]=TextMiningCrisis::radar_reforms_fig(dt_year_radar)
  structure[["review"]]=TextMiningCrisis::radar_structure_fig(dt_year_radar)
  
  list(shocks=shocks,reforms=reforms,structure=structure)
  # htmlwidgets::saveWidget(year_radar,paste0(root_path,"6. Output/Graphs/Text_analysis/Radar_charts/years/radar_",start,"-",end,"_",lexicon,".html"))
})
names(output[["tf-idf_fig_year_radars"]])=buckets

output[["tf-idf_fig_year_radars"]][["c(2005, 2013)"]][["structure"]][["request"]]
output[["tf-idf_fig_year_radars"]][["c(1995, 2005)"]][["structure"]][["Request"]]
output[["tf-idf_fig_year_radars"]][["c(1980, 1995)"]][["structure"]][["Request"]]
output[["tf-idf_fig_year_radars"]][["c(1970, 1980)"]][["structure"]][["Request"]]


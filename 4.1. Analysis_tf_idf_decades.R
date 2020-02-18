
buckets=list(
  #bucket1=c(1960,1970),
  #bucket2=c(1970,1980),
  #bucket3=c(1960,1980),
  #bucket4=c(1980,1995),
  bucket5=c(1995,2005),
  bucket6=c(2005,2016)
  )

output[["tf-idf_fig_year_radars"]]=lapply(buckets,function(x){
  start=x[1]
  end=x[2]
  shocks=list()
  reforms=list()
  structure=list()
  dt_year_radar=tf_by_group(mydata %>% filter(file %in% (mydata %>% filter(type=="request"))$file))
  dt_year_radar=dt_year_radar %>% group_by(Crisis) %>% mutate(year=year(Period)) %>% filter(year>=start & year<end) %>% summarize(word_weight=mean(word_weight),
                                                                                                                                  Period=paste0(min(year),"-",max(year)))
  shocks[["request"]]=country_radar_fig(dt_year_radar)
  reforms[["request"]]=TextMiningCrisis::radar_reforms_fig(dt_year_radar)
  structure[["request"]]=TextMiningCrisis::radar_structure_fig(dt_year_radar)
  
  dt_year_radar=tf_by_group(mydata %>% filter(file %in% (mydata %>% filter(type=="review"))$file))
  dt_year_radar=dt_year_radar %>% group_by(Crisis)%>% mutate(year=year(Period)) %>% filter(year>=start & year<end) %>% summarize(word_weight=mean(word_weight),
                                                                                                                                 Period=paste0(min(year),"-",max(year)))
  shocks[["review"]]=country_radar_fig(dt_year_radar)
  reforms[["review"]]=TextMiningCrisis::radar_reforms_fig(dt_year_radar)
  structure[["review"]]=TextMiningCrisis::radar_structure_fig(dt_year_radar)
  
  dt_year_radar=tf_by_group(mydata %>% filter(file %in% (mydata %>% filter(type=="consultation"))$file))
  dt_year_radar=dt_year_radar %>% group_by(Crisis)%>% mutate(year=year(Period)) %>% filter(year>=start & year<end) %>% summarize(word_weight=mean(word_weight),
                                                                                                                                 Period=paste0(min(year),"-",max(year)))
  shocks[["consultation"]]=country_radar_fig(dt_year_radar)
  reforms[["consultation"]]=TextMiningCrisis::radar_reforms_fig(dt_year_radar)
  structure[["consultation"]]=TextMiningCrisis::radar_structure_fig(dt_year_radar)
  
  list(shocks=shocks,reforms=reforms,structure=structure)
  # htmlwidgets::saveWidget(year_radar,paste0(root_path,"6. Output/Graphs/Text_analysis/Radar_charts/years/radar_",start,"-",end,"_",lexicon,".html"))
})
names(output[["tf-idf_fig_year_radars"]])=buckets

dir.create("../Betin_Collodel/2. Text mining IMF_data/output/figures/Radarchart_by_decade")
# lapply(buckets,function(x){
#   dir.create(paste("../Betin_Collodel/2. Text mining IMF_data/output/figures/Radarchart_by_decade/",x,collapse='_'))
# })

output[["tf-idf_fig_year_radars"]][["c(2005, 2016)"]][["shocks"]][["request"]]
output[["tf-idf_fig_year_radars"]][["c(2005, 2016)"]][["shocks"]][["review"]]
output[["tf-idf_fig_year_radars"]][["c(2005, 2016)"]][["shocks"]][["consultation"]]

# output[["tf-idf_fig_year_radars"]][["c(1995, 2005)"]][["shocks"]][["request"]]
# output[["tf-idf_fig_year_radars"]][["c(1995, 2005)"]][["shocks"]][["review"]]
# output[["tf-idf_fig_year_radars"]][["c(1995, 2005)"]][["shocks"]][["consultation"]]
# 
# output[["tf-idf_fig_year_radars"]][["c(1980, 1995)"]][["shocks"]][["request"]]
# output[["tf-idf_fig_year_radars"]][["c(1980, 1995)"]][["shocks"]][["review"]]
# output[["tf-idf_fig_year_radars"]][["c(1980, 1995)"]][["shocks"]][["consultation"]]
# 
# output[["tf-idf_fig_year_radars"]][["c(1970, 1980)"]][["shocks"]][["request"]]
# output[["tf-idf_fig_year_radars"]][["c(1970, 1980)"]][["shocks"]][["review"]]
# output[["tf-idf_fig_year_radars"]][["c(1970, 1980)"]][["shocks"]][["consultation"]]
# 
# output[["tf-idf_fig_year_radars"]][["c(1960, 1970)"]][["shocks"]][["request"]]
# output[["tf-idf_fig_year_radars"]][["c(1960, 1970)"]][["shocks"]][["review"]]
# output[["tf-idf_fig_year_radars"]][["c(1960, 1970)"]][["shocks"]][["consultation"]]

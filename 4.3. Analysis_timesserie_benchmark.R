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
              ,"TextMiningCrisis"
              ,"SetUpProject",
              "plotly",
              "shinythemes",
              "purrr",
              "wbstats"
)

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## INSTRUCTIONS ##########

# Description:
# The script builds a dataframe with the created tf-idf and other databases of crisis.
# It sources the modules for the different tabs of the shiny app and executes it.

#***************************************************************************************####

output=list()
output[["Session_info"]]=sessionInfo()

# Build working dataframe: ----

TS_compare_benchmark=function(mydata,ctries,var1,var2,benchmark_name="Benchmark",ylabel=NULL,path=NULL){
  #'@title plot own index with respect to historic benchmarks
  #'@description plot own index with respect to historic benchmarks
  #'@param mydata dataset containing the tf-idf of crisis and the 
  #'benchmark qualitative variables 
  #'@param ctries a vector of country codes for which to display the figure
  #'@param var1 the quantative variables of comparison
  #'@param var2 the variable of interest to evaluate
  #'@param ylabel the ylabel for the plot
  #'@return ggplot figure
  #'@author Manuel Betin
  #'@export
  #'

fig=lapply(ctries,function(ctry){
  myfig=mydata %>% 
      filter(ISO3_Code==ctry) %>%
      ggplot()+
      geom_line(aes(x=year,y=(get(var1)-mean(get(var1),na.rm=T))/sd(get(var1),na.rm=T),color=gsub("_"," ",var1)))+
      geom_line(aes(x=year,y=(get(var2)-mean(get(var2),na.rm=T))/sd(get(var2),na.rm=T),color=benchmark_name))+
      theme_bw()+
      labs(y=ylabel,
           x=NULL,
           title=NULL)+
      scale_x_continuous(breaks=seq(1945,2020,5))+ #set y ticks
      scale_color_grey()+
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(size =11,angle=90),
            axis.title.x = element_text(size = 11),
            legend.title = element_blank(),
            axis.title.y = element_text(size=11),
            axis.text.y = element_text(size=11),
            plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5),
            legend.position="bottom")
    if(!is.null(path)){
      myfig + ggsave(filename=paste0("Comparison_benchmark_",ctry,".png"),device = 'png',path=path)
    }else{
      myfig
    }
  })
  names(fig)=ctries
  fig
}

Corr_compare_benchmark=function(mydata,vars){
  dt=mydata%>%ungroup()%>%
    dplyr::select(vars) %>% na.omit() %>%cor() %>% data.frame()
  dt=dt %>% dplyr::select(vars)
  dt
}


# Average value of tf-idf per year:
mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

# Other variables (also standard indicators):
myvars=c("bond.spread","XR_rate_BIS","oil_price","GDPV_XDC_cycle","TTRADE","Troughs",
         "NGDP_WEO","GGXWDG_GDP","GDP_original","SMC.RR","CC.RR","SD_E.RR","SD.BM","SD.debt.Tot.BM","BC.LV",
         "Phase_B","Phase_B2")

Other_data=import("../Betin_Collodel/2. Text mining IMF_data/datasets/comparison/other_data.RData")
names(Other_data)
Other <- Other_data %>%
  dplyr::select(ISO3_Code,year,myvars)%>%
  arrange(ISO3_Code,year) %>% 
  group_by(ISO3_Code, year) %>% 
  summarise_at(vars(myvars), mean, na.rm = TRUE)

mydata=mydata %>% left_join(Other,by=c("ISO3_Code","year"))


mydata=mydata %>% group_by(ISO3_Code) %>% arrange(ISO3_Code,year) %>% mutate(D.XR=XR_rate_BIS/lag(XR_rate_BIS,1)-1,
                                                                             XR_crisis=D.XR>0.2,
                                                                             d.SD.debt.Tot.BM=SD.debt.Tot.BM/lag(SD.debt.Tot.BM,1)-1,
                                                                             d.spread=bond.spread/lag(bond.spread,1)-1,
                                                                             d.expectations=Expectations/lag(Expectations,1)-1,
                                                                             g=NGDP_WEO/lag(NGDP_WEO,1),
                                                                             g_d=ifelse(g<0,1,0),
                                                                             lead_Currency_crisis_severe=lead(Currency_crisis_severe,1),
                                                                             lag_Currency_crisis_severe=lag(Currency_crisis_severe,1),
                                                                             g_crisis=Severe_recession+Soft_recession,
                                                                             d_g_crisis=ifelse(g_crisis>0,1,0),
                                                                             lead_g_crisis=lead(g_crisis,1),
                                                                             lead_Severe_recession=lead(Severe_recession,1)) %>%
  filter(year<2016 & year>1970)


## Compare series with  Benchmark

Eval_benchmarks=list(Financial_crisis=list(countries=c("USA","GBR","FRA"),
                                benchmark="SMC.RR",
                                benchmark_name=" Stock market crash"),
                    Banking_crisis=list(countries=c("USA","GBR","FRA","ARG","MEX","ITA","ISL"),
                                        benchmark="BC.LV",
                                        benchmark_name=" Banking crisis"),
                    Sovereign_default=list(countries=c("BRA","ARG","MEX","URY","GRC","ITA","PRT","ESP","IRL","ISL"),
                                            benchmark="SD.debt.Tot.BM",
                                           benchmark_name=" Sovereign default"),
                    Currency_crisis_severe=list(countries=c("BRA","ARG","MEX","URY","ZAF"),
                                           benchmark="D.XR",
                                           benchmark_name=" Exchange rate depreciation"),
                    Severe_recession=list(countries=c("USA","FRA","MEX","ARG"),
                                          benchmark="g",
                                          benchmark_name=" Nominal GDP growth rate"),
                    Expectations=list(countries=c("USA","FRA","MEX","ARG","ITA","GRC","ESP","CAN","URY"),
                                      benchmark="d.spread",
                                      benchmark_name=" Bond spread")
                    )

Fig_Eval_Benchmark=lapply(names(Eval_benchmarks),function(x){
Corr=Corr_compare_benchmark(mydata,c(x,Eval_benchmarks[[x]][["benchmark"]]))
Fig=TS_compare_benchmark(mydata,Eval_benchmarks[[x]]$countries,x,Eval_benchmarks[[x]][["benchmark"]],
                         benchmark_name=Eval_benchmarks[[x]][["benchmark_name"]],ylabel="St.dev")
list(Corr=Corr,Fig=Fig)
})
names(Fig_Eval_Benchmark)=names(Eval_benchmarks)

#specific comparisons

Fig_Eval_Benchmark[["Currency_crisis_severe"]]$Corr

var="Currency_crisis_severe"
Fig_Eval_Benchmark[[var]]$Fig$MEX + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$URY + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","URY",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))


Fig_Eval_Benchmark[["Expectations"]]$Corr
var="Expectations"
Fig_Eval_Benchmark[[var]]$Fig$MEX + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$FRA + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","FRA",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))


Fig_Eval_Benchmark[[var]]$Fig$ITA + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","ITA",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[["Sovereign_default"]]$Corr
var="Sovereign_default"
Fig_Eval_Benchmark[[var]]$Fig$MEX + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$GRC + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","GRC",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$ARG + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","ARG",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$PRT + 
  ggsave(filename=paste0("Comparison_benchmark_",PRT,"_","MEX",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$IRL + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","IRL",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[["Banking_crisis"]]$Corr
var="Banking_crisis"
Fig_Eval_Benchmark[[var]]$Fig$MEX + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$ISL + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","ISL",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$ITA + 
  ggsave(filename=paste0("Comparison_benchmark_",var,"_","ITA",".png"),device = 'png',path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Comparison/",var))

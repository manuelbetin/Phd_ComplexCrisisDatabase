

#' @title Comparison with benchmark 
#' @description figures comparing the time series 
#' of the indexes of the paper with traditional
#' benchmark variables
#' @author Manuel Betin, Umberto Collodel
#' @return figures in Comparison folder

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions

path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

## load common packages
SetUpProject::load.my.packages(packages)

output=list()
output[["Session_info"]]=sessionInfo()

# Build working dataframe: ----


# Average value of tf-idf per year:
mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

# Other variables (also standard indicators):
myvars=c("bond.spread","XR_rate_BIS","oil_price","GDPV_XDC_cycle","TTRADE","Troughs",
         "NGDP_WEO","GGXWDG_GDP","GDP_original","SMC.RR","CC.RR","SD_E.RR","SD.BM","SD.debt.Tot.BM","BC.LV",
         "Phase_B","Phase_B2")

Other_data=import(paste0(path_data_directory,"/datasets/comparison/other_data.RData"))
#names(Other_data)
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

#--------------------------------------------------
# Currency crisis
var="Currency_crisis_severe"

footnote=c("The darkgrey line represents the normalize difference in long term government bond spread,
           lightgrey line show the tf.idf index of sovereign debt crisis")

cat(footnote, file = paste0(path_data_directory,"/output/figures/Comparison/",var,"/",var,"_footnote.tex"))

Fig_Eval_Benchmark[[var]]$Fig$MEX
ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$URY
ggsave(filename=paste0("Comparison_benchmark_",var,"_","URY",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[["Expectations"]]$Corr


#--------------------------------------------------
# Expectation

var="Expectations"

footnote=c("The darkgrey line represents the normalize difference in long term government bond spread,
           lightgrey line show the tf.idf index of sovereign debt crisis")

cat(footnote, file = paste0(path_data_directory,"/output/figures/Comparison/",var,"/",var,"_footnote.tex"))

Fig_Eval_Benchmark[[var]]$Fig$MEX 
ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$FRA
ggsave(filename=paste0("Comparison_benchmark_",var,"_","FRA",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$ITA
ggsave(filename=paste0("Comparison_benchmark_",var,"_","ITA",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

#--------------------------------------------------
#sovereign defaut

var="Sovereign_default"

footnote=c("The darkgrey line represents the normalize amount of debt in default and or restructuring from Beers and Maravella (2017),
           lightgrey line show the tf.idf index of sovereign debt crisis")

cat(footnote, file = paste0(path_data_directory,"/output/figures/Comparison/",var,"/",var,"_footnote.tex"))

Fig_Eval_Benchmark[[var]]$Fig$MEX
ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$GRC
ggsave(filename=paste0("Comparison_benchmark_",var,"_","GRC",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$ARG
ggsave(filename=paste0("Comparison_benchmark_",var,"_","ARG",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$PRT
ggsave(filename=paste0("Comparison_benchmark_",var,"_","PRT",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$IRL
ggsave(filename=paste0("Comparison_benchmark_",var,"_","IRL",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[["Banking_crisis"]]$Corr

#--------------------------------------------------
# Banking crisis


footnote=c("The darkgrey line represents the binary variable for Banking crisis from Laven and Valencia (2012),
           lightgrey line show the tf.idf index of banking crisis")

var="Banking_crisis"

cat(footnote, file = paste0(path_data_directory,"/output/figures/Comparison/",var,"/",var,"_footnote.tex"))

Fig_Eval_Benchmark[[var]]$Fig$MEX
ggsave(filename=paste0("Comparison_benchmark_",var,"_","MEX",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$ISL
ggsave(filename=paste0("Comparison_benchmark_",var,"_","ISL",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$ITA 
ggsave(filename=paste0("Comparison_benchmark_",var,"_","ITA",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))

Fig_Eval_Benchmark[[var]]$Fig$USA
ggsave(filename=paste0("Comparison_benchmark_",var,"_","USA",".png"),device = 'png',path=paste0(path_data_directory,"/output/figures/Comparison/",var))


######## Description: the script generates graph for the time series of some less standard events

# Average data over year:

mydata <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)

# Notable examples of social crises -------

# Labor bill strikes and gilet jaunes movement:

get_timeserie=function(mydata,ctry,shocks,lowerbound=0,path=NULL){
  #' @title plot event study of crisis by country
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param ctry the country to which display the figure
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  figs=lapply(shocks,function(x){
     myfig=mydata %>% 
      filter(ISO3_Code == ctry) %>% ungroup() %>%
      mutate(proba=ifelse(get_prob(get(x))==1,year,NA),
             max=ifelse(max(get(x),na.rm=T)==get(x),get(x),NA)) %>%
      ggplot(aes(year, get(x) , group =1)) +
      geom_vline(aes(xintercept=proba),color="lightgrey",size=7) +
      geom_line(col = "darkblue",alpha=0.6, size = 1.2) +
      geom_point(col = "darkblue",size=1.8) +
      geom_text(aes(x=proba,y=max(get(x),na.rm=T)*2/3,label=proba),angle = 90,size=3.5)+
      theme_bw()+
       labs(y="Term frequency (%)",
            x=NULL,
            title=NULL)+
       #lims(y=c(ymin,ymax))+
       scale_x_continuous(breaks=seq(1945,2020,5))+ #set y ticks
       theme(panel.grid.minor = element_blank(),
             axis.text.x = element_text(size =15,angle=90),
             axis.title.x = element_text(size = 11),
             axis.title.y = element_text(size=15),
             axis.text.y = element_text(size=15),
             plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
             plot.subtitle =element_text(size =7, hjust = 0.5),
             legend.position="none")
    
    if(!is.null(path)){
      myfig + ggsave(filename=paste0(ctry,"_",x,".png"),device = 'png',path=path, width = 7, height = 4, dpi = 300)
    }else{
      myfig
    }
    })
  names(figs)=shocks
  figs
}
    
# SARS and COVID19
ctry="CHN"
var="Epidemics"
get_timeserie(mydata,ctry,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",var))

# France and China social protests

ctry=c("CHN","FRA")
var="Social_crisis"
ctry %>% 
map(~ get_timeserie(mydata,.x,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",.x)))


#Natural disasters in Haiti and Mexico
ctry=c("HTI","MEX")
var="Natural_disaster"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",.x)))


#Migration in COlombia, Germany, Italy and Lebanon
ctry=c("COL","DEU","ITA","LBN")
var="Migration"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",.x)))


#Trade crisis US-China 
ctry=c("CHN","USA")
var="Trade_crisis"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",.x)))


#Financial crisis 
ctry="USA"
var="Financial_crisis"
get_timeserie(mydata,ctry,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",var))


# recession in Greece and USA
ctry=c("GRC","USA")
var="Severe_recession"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",.x)))



# Housing crisis in USA and Spain
ctry=c("ESP","USA")
var="Housing_crisis"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",.x)))


# Sovereign default in argentina and Greece:
ctry=c("ARG","GRC")

var="Sovereign_default"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",.x)))



#Sovereign default in Greece
ctry="FRA"
var="Social_crisis"
get_timeserie(mydata,ctry,var,path=paste0("../Betin_Collodel/2. Text mining IMF_data/output/figures/Time series by country/",var))






#' @title Figures event studies of crisis
#' @description produce the figures of the 
#' time series of the index for a selected 
#' group of countries and crisis.
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder 
#' Time series by country

path_data_directory="../Betin_Collodel/2. Text mining IMF_data"


# Average data over year:

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
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
    
# SARS and COVID19 and malaria
ctry=c("CHN","HTI")
var="Epidemics"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of epidemics crisis in China and Haiti 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a epidemic crisis. China as recurrent
           episodes of epidemics since 1995 with the SARS outbreak in 2003-2004, the respiratory syndrom in 2008 and
           the recent COVID 19 outbreak. Haity has a long history of epidemics with severe episodes all along its history and 
in particular the AIDS outbreak in the 1970s and the Cholera outbreak in 2009-2010.
           ")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))


# France and China social protests

ctry=c("CHN","FRA")
var="Social_crisis"
ctry %>% 
map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of Social crisis in France and China. 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a Social crisis. The highest social 
           crisis of both countries occured in the recent period. The gilet jaune crisis in France generated a large 
           spike in Dicember 2019. In China the Honk Kong protests appear as the major social event for China.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))


#Natural disasters in Haiti and Mexico
ctry=c("HTI","MEX")
var="Natural_disaster"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))


#Migration in COlombia, Germany, Italy and Lebanon
ctry=c("COL","DEU","ITA","LBN")
var="Migration"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of migration crisis in Germany and Lebanon. 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a migration crisis. The highest migration  
           crisis of both countries occured in the recent period. Germany displays a steady increase of the index since 2015
           and the first inflows of refugees related to the Syrian war. Also following the Syrian war,Lebanon display a first
           spike in 2011 at the start of the conflict with a rebounce in 2015 and 2016.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))

#Trade crisis US-China 
ctry=c("CHN","USA")
var="Trade_crisis"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of trade crisis in The United States and China. 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a trade crisis. The highest trade 
           crisis of both countries started in the 2016 following the restriction imposed by Trump and the excalating tension
           between the two countries.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))

#Financial crisis 
ctry="USA"
var="Financial_crisis"
get_timeserie(mydata,ctry,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var))


# recession in Greece and USA
ctry=c("GRC","USA")
var="Severe_recession"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of severe recessions in Greece and The United States. 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a severe recession.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))

# Housing crisis in USA and Spain
ctry=c("ESP","USA")
var="Housing_crisis"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of housing crisis in the United States and Spain. 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a housing crisis. While the US housing crash example is an illustrous one, also the Spanish 
housing market collapsed in the same period: in the years 1997???2006, the price of housing in Spain had risen about 150% in nominal terms, equivalent to 100% growth in real terms to
           then plunge dramatically starting 2008.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))


# Sovereign default in argentina and Greece:
ctry=c("ARG","GRC")

var="Sovereign_default"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))


footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of sovereign debt crisis in Argentina and Greece 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a sovereign debt crisis. Argentina displays
           a long history of debt distress  with five major episodes spanning over several years and spiking locally in 1964, 1991,
           2005 and 2017-2018. The major events of debt crisis in Greece occur in 2010 and 2015 and related to the euro area
           sovereign debt crisis period. Low intense occurence also occured during the early 2000 and 1980 but with significantly 
           lower intensity.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))


#Natural disaster 

ctry=c("JPN","LKA")
var="Natural_disaster"
#get_timeserie(mydata,"LKA","Natural_disaster")

ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of natural disasters in Japan and Sri Lanka. 
           Grey shaded highligh positive tf.idf corresponding to the occurence of a natural disasters. Japan has a long history
           of natural disaster due to its geographical situation among major recent event we observe the spike related to the 
           Kobe earthquake and the Fukushima Tsunami and Nuclear consequences in 2011-2012. Sri Lanka shows a constant 
           vulnerability to natural disaster with an event happening almost every year since 1970. Among the largest events
           we fine the 2004 tsunami and the 2017 flood that severily hit the country. ")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))


#Wars in Colombia and India
ctry=c("COL","LKA")
var="Wars"
ctry %>% 
  map(~ get_timeserie(mydata,.x,var,path=paste0(path_data_directory,"/output/figures/Time series by country/",var)))

footnote=c("Blue lines corresponds to the tf.idf index and measure the intensity of conflicts in Colombia and Sri Lanka. Although the conflict
           between the Colombian government and the Revolutionary Armed Forces of Colombia (FARC) as well as other guerilla forces started in 1960, it intensified in the mid-1990s 
           as a consequence of the higher wealth accumulated by terrorist group through drug-related activities. The index peaks again in 2017 when the peace referendum between the governemnt 
           and FARC rebels failed as 'No' gained the majority. In Sri Lanka, the conflict between the government
           and the Liberation Tigers of Tamil Eelam (LTTE) wrought havoc the country: Sri Lanka was, after the indipendence from India, the country with
           the highest level of alphabetization in South-East Asia and one of the richest. The war decimated the population and hit sevely the economy. The official end-year of the conflict is 2009:
this complies with the gap of the index after 2010. The index starts rising again with the increase in the number of attacks against the Christian part of the population and
peaks in 2019 the with Sri Lanka Easter bombings in the capital Colombo."
)

cat(footnote,file=paste0(path_data_directory,"/output/figures/Time series by country/",var,"/",var,"_footnote.tex"))




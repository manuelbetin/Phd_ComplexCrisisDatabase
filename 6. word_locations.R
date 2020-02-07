library(dplyr)
library(TextMiningCrisis)
library(ggplot2)
rio::import("temp/corpus/corpus_ARG.RData")
corpus=rio::import("temp/corpus/corpus_ARG.RData")

tf_with_pages=function(corpus,file_names,list_keywords){
  dt=lapply(file_names,function(file_name){
    print(file_name)
    mydoc=corpus[[file_name]]$file
    names(mydoc)=1:length(mydoc)
    mydoc=as.list(mydoc)
    categories=lapply(names(list_keywords),function(category){
      dt_by_category=eval_pages(mydoc,list_keywords[[category]],brute_freq = F)
      dt_by_category= data.frame(dt_by_category)
      names(dt_by_category)=category
      dt_by_category
    })
    categories=do.call(cbind,categories)
    categories$page=rownames(categories)
    categories$file=file_name
    categories$TotPages=length(mydoc)
    categories  
  })
  dt=do.call(rbind,dt)
  rownames(dt)=NULL
  dt=dt %>% dplyr::select(file,page,TotPages,everything()) %>%
    mutate_at(names(list_keywords),as.numeric) %>%
    mutate(location=round(as.numeric(page)/as.numeric(TotPages),1))
  return(dt)
}

a=data.frame(quantile(1:4))[,1]

files=names(corpus)[263:337]

files=names(corpus)[263:266]

#files=c("ARG_1998-01-14_request","ARG_2003-02-05_request")

targets=c("Fiscal_outcomes","Banking_crisis","Currency_crisis","Sovereign_default")

list(EBM=c("minute"))
dt=tf_with_pages(corpus=corpus,file=files,list_keywords=key_words_crisis()[targets])

dt2=dt %>% tidyr::gather("category","value",-c(file,page,location,TotPages)) %>% mutate(Period=as.Date(substr(file,5,14)))

ggplot(dt2 %>% filter(location<=1))+
  geom_bar(stat="identity",aes(x=Period,y=value,fill=category))+
  geom_point(aes(x=Period,y=value,color=category))

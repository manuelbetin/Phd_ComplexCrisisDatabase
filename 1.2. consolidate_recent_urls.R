library(rvest)
find_name_from_title=function(dt){
  
  if(!any(c("title","year") %in% names(dt))){
    print("please provide a valide database containing at least the columns title and year")
    dt
  }else{
    dt=dt %>% mutate(title2=str_replace(title,":","-")) %>% separate(title2,into="country",sep="-") %>% dplyr::select(iso3,country,period,title,everything())
    
    dt= dt  %>% mutate(country=str_trim(gsub('[^ -~]', '', country),"both"))
    
    ctries=countrycode::countrycode(list_countries(),origin="iso3c",destination="country.name") %>% tolower()
    
    nonstandard_ctrynames=c(COD="zaire",SOM="somalia",YEM="yemen arab republic","yugoslavia",CIV="ivory coast",WSM="western samoa",HUN="hungarian people's republic",KOR="korea",
                            MMR="burma",VCT="st. vincent and the grenadines",GMB="the gambia",CIV="cote d'ivoire",COD="people's republic of the congo",CHN="people's republic of china",
                            EGY="arab republic of egypt",MOZ="people's republic of mozambique",TTO="trinidad and tobago",STP="sao tome and principe",LAO="lao people's democratic republic",
                            MOZ="republic of mozambique",POL="republic of poland",CZE="czech and slovak federal republic",RUS='russian federation',CZE="czech republic",SVK="slovak republic",
                            LVA='republic of latvia',KGZ="kyrgyz republic",MDA="republic of moldova",VNM="viet nam",LTU="republic of lithuania",EST="republic of estonia",KAZ="republic of kazakhstan",
                            MKD="former yugoslav republic of macedonia",COG="republic of congo",HRV="republic of croatia",ARM="republic of armenia",BLR="republic of belarus",UZB="republic of uzbekistan",
                            AZE="azerbaijan republic",GEO="republic of georgia",KAZ="republic of kazakstan",BIH="republic of bosnia and herzegovina",YEM="republic of yemen",TJK="republic of tajikistan",
                            BIH="bosnia and herzegovina",KOR="republic of korea",KNA="st. kitts and nevis",GNQ="guinea bissau",MEX="mexico <U+0097> arrangement under the flexible credit line",
                            MEX="mexico<U+0097>review under the flexible credit line arrangement",COL="colombia<U+0097>review under the flexible credit line arrangement")
    
    nonstandard_ctrynames2=as.data.frame(nonstandard_ctrynames)
    nonstandard_ctrynames2$iso3c=names(nonstandard_ctrynames)
    names(nonstandard_ctrynames2)=c("iso3_new","iso3c")
    nonstandard_ctrynames2=nonstandard_ctrynames2 %>% mutate(iso3_new=as.character(iso3_new))
    
    dt=dt %>% mutate(iso3_error=ifelse(!country %in% c(ctries,nonstandard_ctrynames),country,""),
                     iso3_new=as.character(ifelse(country %in% c(ctries,nonstandard_ctrynames),country,"")))
    
    dt=dt %>% left_join(nonstandard_ctrynames2,by=c("iso3_new"))
    
    #correct manually some cases and transform to iso3c
    dt=dt %>% mutate(iso3c=ifelse(is.na(iso3c),countrycode::countrycode(iso3_new,origin="country.name",destination="iso3c"),iso3c),
                     iso3c=ifelse(str_detect(iso3,"mexico"),"MEX",iso3c),
                     #iso3c=ifelse(str_detect(title,"germany"),"DEU",iso3c),
                     iso3c=ifelse(str_detect(iso3,"philippines"),"PHL",iso3c),
                     iso3c=ifelse(str_detect(iso3,"macedonia"),"MKD",iso3c),
                     iso3c=ifelse(str_detect(iso3,"yugoslavia"),"YUG",iso3c))
    
    
    mycountries=c(ctries,nonstandard_ctrynames)
    for(j in 1:length(mycountries)){
      iso3ccode=countrycode::countrycode(mycountries[j],origin="country.name",destination="iso3c")
      dt=dt%>%mutate(iso3c=ifelse(is.na(iso3c) & str_detect(title,mycountries[j]),iso3ccode,iso3c))
    }
    
    
    
    dt=dt %>% dplyr::select(-c(iso3_new,iso3_error)) %>% rename(iso3_from_title=iso3c) %>%
      dplyr::select(iso3,country,iso3_from_title,period,year,pdf,everything())
    
    
  }
  dt
}

get_links=function(){
    #' download the url of the country reports from the imf website
    #' @description find the urls to download the imf country reports
    #' @author Manuel Betin
    #' @return dataframe with the title of the document, the name of the file and the url of the file
    #' @export 
    #' 
    #' 
  
  #find the hompage of the IMF
  homepage=read_html("https://www.imf.org/en/Publications/CR/")
  #find the number of pages
  npages=homepage %>% html_nodes(xpath = "/html/body/div[3]/main/article/div[3]/div[1]/p/text()") %>% html_text() 
  npages=str_extract(npages,"\\d\\d\\d")[2] %>% as.numeric()
  npages=200
  progress = dplyr::progress_estimated(npages)
  
  urls=lapply(1:npages,function(x){
    tictoc::tic(paste0("page: ",x,"/",npages))
    #get specific page
    homepage=read_html(paste0("https://www.imf.org/en/Publications/CR/?page=",x))
    results=homepage %>% html_nodes(xpath = '/html/body/div[3]/main/article/div[3]')
    #get url of the files
    href=results %>% html_nodes("a") %>% html_attr("href")
    href=data.frame(href) 
    href=href %>% filter(!str_detect(href,"www.imf.org")) %>%
      mutate(url=paste0("https://www.imf.org/",href))
    myurl=href$url[6]
    dt=lapply(href$url,function(myurl){
      dt=try({ 
        docpage=read_html(myurl)
        docname=docpage %>% html_nodes(css=".conf") %>% html_text()
        docdate=docpage %>% html_nodes(css=".pub-lang .pub-desc") %>% html_text()
        docdate=docdate[1]
        docdate=str_remove_all(docdate,"\r\n") %>% str_remove_all(" ")
        docurl= docpage %>% html_nodes(css = ".piwik_download") %>% html_attr('href')
        docurl=paste0("https://www.imf.org",docurl)
        data.frame(title=docname,period=docdate,pdf=docurl)}, silent = T)
      if ("try-error" %in% class(dt)) {
        cat(crayon::red(paste(docname, ": Error in path file: ", 
                              myurl, sep = "")))
        data.frame(title=NA,period=NA,pdf=myurl)
      }
      dt
    })
    dt=do.call(rbind,dt)
    dt=dt %>% mutate(name_file=substr(pdf,81-16,81-5),
                     pdf=as.character(pdf),
                     iso3=substr(name_file,1,3),
                     period=as.Date(period,format=c("%B%d,%Y")),
                     year=year(period)) %>% dplyr::select(title,pdf,iso3,period,year,name_file)
    progress$pause(0.01)$tick()$print()
    tictoc::toc()
    dt
    
  })
  urls=do.call(rbind,urls)
    return(urls)
}

IMF_links=get_links()
IMF_links=IMF_links %>% find_name_from_title()
IMF_links= IMF_links %>% mutate(name_file=str_extract(pdf,"[:alpha:][:alpha:][:alpha:][:alpha:][:alpha:]\\d+(?=.ashx?)"),
                                iso3=countrycode::countrycode(country,origin="country.name",destination="iso3c"))

IMF_links=IMF_links %>% mutate(title=tolower(title)) %>% dplyr::select(title,pdf,iso3,period,year)

rio::export(IMF_links,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/recent_IMF_urls.RData")
#IMF_links=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/recent_IMF_urls.RData")



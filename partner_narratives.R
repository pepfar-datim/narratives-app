require(datimvalidation)
require(httr)
require(jsonlite)
require(magrittr)
require(purrr)
require(dplyr)
require(tibble)
require(seqinr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidytext)
require(config)
require(tibble)

getOperatingUnits<-function(config) {

  cached_file<-paste0(config$deploy_location,"/operating_units.Rds")
  
  if ( file.exists(cached_file) ) {return(readRDS("operating_units.Rds"))}
  
d<-paste0(getOption("baseurl"),"api/dataStore/dataSetAssignments/orgUnitLevels") %>% 
  URLencode(.) %>% 
  httr::GET(.) %>% 
  httr::content(.,"text") %>% 
  jsonlite::fromJSON(.)

names3<-purrr::map(d,purrr::pluck("name3")) %>% unlist(.)
names4<-purrr::map(d,purrr::pluck("name4")) %>% unlist(.)
ou_map<-data.frame(ou=names3,country=names4,stringsAsFactors = FALSE)
ou_map$country<-ifelse(nchar(ou_map$country) == 0,ou_map$ou,ou_map$country)
ou_map %<>% dplyr::arrange(ou,country) 

ous<-paste0(getOption("baseurl"),"api/organisationUnits?filter=level:lt:5&fields=id,name&paging=false") %>% 
  URLencode(.) %>% 
  httr::GET(.) %>% 
  httr::content(.,"text") %>% 
  jsonlite::fromJSON(.) %>% 
  purrr::pluck("organisationUnits")

ou_map %>% 
  dplyr::left_join(ous,by=c("country"="name")) %>% 
  rename(country_id = id) %>% 
  dplyr::left_join(ous,by=c("ou"="name")) %>% 
  rename(ou_id = id) }

getUSGNarrativeMetadata<-function(config) {
  
  cached_file<-paste0(config$deploy_location,"/usg_narrative_metadata.Rds")
  
  if ( file.exists(cached_file) ) {return(readRDS(cached_file))}
  
  paste0(getOption("baseurl"),"api/dataSets/wkdCW3M4zYT?fields=id,dataSetElements,organisationUnits") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("dataSetElements") %>% 
    purrr::pluck("dataElement")
  


}

getPartnerNarrativeMetadata<-function()  {

  
  cached_file<-paste0(config$deploy_location,"/partner_metadata.Rds")
  
  if ( file.exists(cached_file) ) {return(readRDS(cached_file))}
  
  paste0(getOption("baseurl"),"api/dataSets/x5lMwEHWSds?fields=id,dataSetElements,organisationUnits") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("dataSetElements") %>% 
    purrr::pluck("dataElement")
  
}

getAllMechanisms<-function(operating_units) {
  
  cached_file<-paste0(config$deploy_location,"/all_mechs.Rds")
  
  if ( file.exists(cached_file) ) {return(readRDS(cached_file))}
  
  mechs_cocs_cos<-paste0(getOption("baseurl"),"api/",api_version(),"/categoryOptionCombos?filter=categoryCombo.id:eq:wUpfppgjEza&fields=id,categoryOptions[id]&paging=false") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.,simplifyDataFrame = TRUE) %>% 
    purrr::pluck("categoryOptionCombos") %>% 
    dplyr::mutate(categoryOptions = unlist(categoryOptions))

  
operating_units<-operating_units %>% 
  dplyr::select(ou,ou_id) %>% 
  dplyr::distinct()

  mechs<-paste0(getOption("baseurl"),"api/sqlViews/fgUtV6e9YIX/data.csv") %>% 
    utils::URLencode() %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    readr::read_csv() %>% 
    dplyr::left_join(operating_units,by=c("ou"))
  
  mechs %<>% dplyr::inner_join(mechs_cocs_cos,by=c("uid" = "id")) %>% 
    dplyr::filter(!is.na(ou_id))
  
  mechs
}

assemblePartnerNarrativeURL<-function(ou,selected_mechs,period) {

  
  base_url<-paste0(getOption("baseurl"),"api/analytics?")
  mechanisms_bit<-paste0("dimension=SH885jaRe0o")
  period_bit<-paste0("&filter=pe:", period)
  des<-getPartnerNarrativeMetadata() %>%  unlist()
  de_bit<-paste0("&dimension=dx:",paste(des,sep="",collapse=";"))
  ou_bit<-paste0("&filter=ou:", ou)
  end_bit<-"&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false&outputIdScheme=uid"
  paste0(base_url,mechanisms_bit,de_bit,ou_bit,period_bit,end_bit)
  
}

assembleUSGNarrativeURL<-function(ou,period,config) {
  
  
  base_url<-paste0(getOption("baseurl"),"api/analytics?")
  period_bit<-paste0("&filter=pe:", period)
  des<-getUSGNarrativeMetadata(config) %>% unlist()
  de_bit<-paste0("&dimension=dx:",paste(des,sep="",collapse=";"))
  ou_bit<-paste0("&filter=ou:", ou)
  end_bit<-"&filter=ao:xYerKDKCefk&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false"
  paste0(base_url,de_bit,ou_bit,period_bit,end_bit)
  
}

d2_analyticsResponse <- function(url,remapCols=TRUE) {
  d <- jsonlite::fromJSON(httr::content(httr::GET(url), "text"))
  
  if ( NROW(d$rows) > 0 ) {
    metadata <- do.call(rbind,
                        lapply(d$metaData$items,
                               data.frame, stringsAsFactors = FALSE)) %>% mutate(., from = row.names(.))
    remapMeta <-
      function(x) {
        plyr::mapvalues(x, metadata$from, metadata$name, warn_missing = FALSE)
      }
    
    d<-as.tibble(d$rows) %>% `names<-`(., d$headers$column)
    if(remapCols == TRUE) {
      d<-plyr::colwise(remapMeta)(d)
    }
    return(d) } else {
      return(NULL)
    }
}


generateWordCloud<-function(partner_data,usg_data) {
  
  all_text<-rbind(partner_data$Value,usg_data$Value)
  myCorpus <- Corpus(VectorSource(all_text))
  docs <- myCorpus %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  set.seed(1234) # for reproducibility 
  wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

sentimentTable<-function(partner_data) {
  partner_data %>% 
    dplyr::select(code,Value) %>% 
    unnest_tokens(word,Value) %>% 
    filter(word != 'positive') %>% 
    inner_join(get_sentiments("nrc")) %>% 
    dplyr::count(code,sentiment) %>% 
    group_by(code) %>% 
    arrange(-n) %>% 
    slice(1:5) %>% 
    mutate(n=row_number()) %>% 
    ungroup() %>% 
    mutate(sentiment = firstup(sentiment)) %>% 
    spread(n,sentiment) %>% 
    dplyr::rename(`Mechanism` = code)
}

  


#Automation section
#Loop through by OU


 datimvalidation::loadSecrets("/home/jason/.secrets/datim-prod.json")
 setwd("/home/jason/development/narratives-app/")
 config <- config::get()
 options("baseurl" = config$baseurl)


 operating_units<-getOperatingUnits(config)
 all_mechs<-getAllMechanisms(operating_units)
 original_wd<-config$deploy_location

 ous_unique<-unique(operating_units$ou_id)

 for ( i in 1:length(ous_unique)) {

 this_ou<-ous_unique[i]
   selected_countries <- operating_units[operating_units$ou_id == this_ou,]
   print(paste("Making report for ",selected_countries[1,"ou"]))

   this_period<-"2020Q1"

   ou_mechs<-all_mechs %>%
     dplyr::filter(ou_id == this_ou)

   if ( NROW(ou_mechs) > 1 ) {

     this_url<-assemblePartnerNarrativeURL(ou = selected_countries$country_id,
                                           selected_mechs = ou_mechs$uid,
                                         period = this_period) %>%
       stringr::str_remove_all(.,"\n")

     partner_data<-list()
     for (j in 1:NROW(selected_countries)) {

       this_data<-d2_analyticsResponse(this_url[j])
       if ( NROW(this_data) > 0) {
         this_data$country<-selected_countries$country[j]
         partner_data[[j]]<-this_data

       } else {
       partner_data[[j]]<-NULL
       }
     }

     if (!is.null(partner_data)) {
       partner_data<-do.call(rbind.data.frame,partner_data)
       mech_codes<-stringr::str_split(partner_data$`Funding Mechanism`," - ") %>%
         map(.,purrr::pluck(2)) %>%
         unlist()

       partner_data$code<-mech_codes

       partner_data %<>% dplyr::inner_join(all_mechs,by="code") %>%
         dplyr::arrange(country,code)
     }


     #USG Narratives

     this_url<-assembleUSGNarrativeURL(ou = selected_countries$country_id,
                                       period = this_period,config)

     usg_data<-list()
     for (j in 1:NROW(selected_countries)) {
       this_data<-d2_analyticsResponse(this_url[j])
       if ( NROW(this_data) > 0) {

         this_data$country<-selected_countries$country[j]
         usg_data[[j]]<-this_data

       } else {
         usg_data[[j]]<-NULL
       }
     }

     usg_data<-do.call(rbind.data.frame,usg_data)

       tryCatch(rmarkdown::render(
         paste0(original_wd, "/partner_narratives_template.Rmd"),
         output_file = paste0(original_wd, "/",this_period,"/", operating_units[operating_units$ou_id == this_ou,"ou"][[1]], ".pdf")
       ),
       error = function(e) {
         print(paste("Could not render report for ", operating_units[operating_units$ou_id == this_ou,"ou"][[1]]))
       } )


   }
 }

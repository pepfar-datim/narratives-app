
#Initiate logging
logger <- flog.logger()
#Load the local config file
config <- config::get()
options("baseurl" = config$baseurl)
flog.appender(appender.file(config$log_path), name="narratives")


DHISLogin <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(config$baseurl, "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}


getOperatingUnits<-function(config) {
  
  
  ou_map<-paste0(getOption("baseurl"),"api/dataStore/dataSetAssignments/orgUnitLevels") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    dplyr::bind_rows() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate( country = dplyr::case_when( nchar(name4) == 0 ~ name3,
                                           TRUE ~ name4),
                   ou = name3) %>% 
    dplyr::select(ou,country)
  
  ous<-paste0(getOption("baseurl"),"api/organisationUnits?filter=level:lt:5&fields=id,name&paging=false") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("organisationUnits")
  
  ou_map %>% 
    dplyr::inner_join(ous,by=c("country"="name")) %>% 
    dplyr::rename(country_id = id) %>% 
    dplyr::inner_join(ous,by=c("ou"="name")) %>% 
    dplyr::rename(ou_id = id) %>% 
    dplyr::arrange(ou,country)}


getPeriods<-function() {
  
  qtrseq<-seq(as.Date("2017-01-01"), by="quarter", length.out = 30)
  qtrseq<-qtrseq[qtrseq <= Sys.Date()]
  qtrseq<-rev(paste0(as.POSIXlt(qtrseq)$year+1900, quarters(qtrseq)))
  qtrseq[2:length(qtrseq)]
}


getUSGNarrativeDataElements<-function() {
  
  
  paste0(getOption("baseurl"),"api/dataSets/wkdCW3M4zYT?fields=id,dataSetElements,organisationUnits") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("dataSetElements") %>% 
    purrr::pluck("dataElement")
  
  
  
}

getNarrativeDataElements<-function(fiscal_year, type="Results") {
  
  
  des<- paste0(getOption("baseurl"),"api/dataElementGroups?filter=name:like:Narratives&paging=false&fields=id,name,dataElements[id,shortName]") %>% 
    httr::GET() %>% httr::content("text") %>% jsonlite::fromJSON() %>% 
    purrr::pluck("dataElementGroups") %>%
    tidyr::unnest_longer(.,"dataElements",simplify = TRUE) %>% dplyr::mutate(de_name = dataElements$shortName, de_uid = dataElements$id) %>% 
    dplyr::select(-id,-dataElements) %>% 
    mutate(year =   ( stringr::str_split(name," ") ) %>% purrr::map(purrr::pluck(1)) %>% unlist() ) %>% 
    dplyr::filter(stringr::str_detect(name,"^20")) %>% 
    dplyr::mutate(type = ifelse(stringr::str_detect(name,"Result"),"Results","Targets")) %>% 
    mutate(technical_area =   ( stringr::str_split(de_name," ") ) %>% purrr::map(purrr::pluck(1)) %>% unlist() ) 
  
  getListElement<-function(x,n) tryCatch(str_trim(str_split(x,","))[[n]],  error = function(e) return(NA))
  #Tech area
  #Classification
 support_type<-  
    gsub("\\)", "", gsub(
      "\\(",
      "",
      stringr::str_extract_all(des$de_name, "\\(.+\\)"))) %>% 
   tibble::enframe() %>% 
   tidyr::separate(value,
                   into = c("num_denom", "support_type"),
                   sep = ", ") %>% 
   dplyr::mutate(support_type = stringr::str_remove(support_type, "[ _]NARRATIVE")) %>% 
   dplyr::select("support_type")
  
   dplyr::bind_cols(des,support_type) %>% 
    dplyr::filter(type == type) %>% 
    dplyr::filter(year == fiscal_year)
  
  
  
}


getCurrentFiscalYear<-function(this_date = Sys.Date()) {

  if (lubridate::quarter(this_date) == 1) {
    this_year<-lubridate::year(this_date) -1
  } else {
    this_year<-lubridate::year(this_date) 
  }
    this_year
}


getCurrentFiscalQuarter<-function(this_date = Sys.Date()) {
  
  if (lubridate::quarter(this_date) == 1) {
    return(4) }

    if ( lubridate::quarter(this_date) %in% c(2,3,4) ) {
      lubridate::quarter(this_date) - 1
    }
  

}

convertFYQuarterCalendarQuarter<-function(fiscal_year,fiscal_quarter) {
  
  fiscal_year<-as.integer(fiscal_year)
  print(paste0("Fiscal year",fiscal_year))
  fiscal_quarter<-as.integer(fiscal_quarter)
  print(paste("Fiscal quarter",fiscal_quarter))
  if ( !( fiscal_quarter %in% c(1,2,3,4) ) ) {stop("Invalid fiscal quarter")} 
  if (fiscal_quarter == 1) {
    calendar_quarter  <-  4
    calendar_year <- fiscal_year - 1
  } else {
    calendar_quarter <- fiscal_quarter - 1
    calendar_year <- fiscal_year
  }
  
  paste0(calendar_year,"Q",calendar_quarter)
  
}

assemblePartnerNarrativeURL<-function(ou,fiscal_year,fiscal_quarter,all_des) {
  
  this_period<-convertFYQuarterCalendarQuarter(fiscal_year , fiscal_quarter )
  base_url<-paste0(getOption("baseurl"),"api/analytics?")
  
  mechanisms_bit<-paste0("dimension=SH885jaRe0o")

  period_bit<-paste0("&filter=pe:", this_period)
  de_bit<-paste0("&dimension=dx:",paste(all_des,sep="",collapse=";"))
  ou_bit<-paste0("&dimension=ou:", paste(ou,sep="",collapse=";"))
  end_bit<-"&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false&outputIdScheme=uid"
  paste0(base_url,mechanisms_bit,de_bit,ou_bit,period_bit,end_bit)
  
}


assembleUSGNarrativeURL<-function(ou, fiscal_year, fiscal_quarter) {
  
  this_period<-convertFYQuarterCalendarQuarter(fiscal_year , fiscal_quarter )
  
  base_url<-paste0(getOption("baseurl"),"api/analytics?")
  period_bit<-paste0("&filter=pe:", this_period)
  des<-getUSGNarrativeDataElements() %>% unlist()
  de_bit<-paste0("&dimension=dx:",paste(des,sep="",collapse=";"))
  ou_bit<-paste0("&filter=ou:", ou)
  end_bit<-"&filter=ao:xYerKDKCefk&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false"
  paste0(base_url,de_bit,ou_bit,period_bit,end_bit)
  
}

getUserMechanisms<-function() {
  
  mechs<-paste0(getOption("baseurl"),"api/",api_version(),"/categoryOptionCombos?filter=categoryCombo.id:eq:wUpfppgjEza&fields=id,code,name,categoryOptions[id,organisationUnits[id,name]&paging=false") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.,simplifyDataFrame = TRUE) %>% 
    purrr::pluck("categoryOptionCombos") %>% 
    tidyr::unnest( cols=c("categoryOptions"), names_sep=".") %>% 
    tidyr::unnest(., cols=c("categoryOptions.organisationUnits"), names_sep=".") %>% 
    dplyr::select(mech_code = code,
                  mech_name = name,
                  categoryoptioncomboid = id,
                  category_option_id = categoryOptions.id,
                  orgunit_name = categoryOptions.organisationUnits.name,
                  orgunit_id = categoryOptions.organisationUnits.id)
  

  #Agency map
  agencies_cos<-paste0(getOption("baseurl"),"api/",api_version(),"/categoryOptionGroupSets/bw8KHXzxd9i?fields=categoryOptionGroups[id,name,categoryOptions[id]]&paging=false") %>% 
   URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("categoryOptionGroups") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(cat_options = length(categoryOptions)) %>% 
    dplyr::filter(cat_options > 0) %>% 
    dplyr::select(-cat_options) %>% 
    tidyr::unnest(.,cols = c(categoryOptions),names_sep=".") %>% 
    dplyr::rename(agency_name = name, agency_id = id, category_option_id = categoryOptions.id)
  
  #Partner map
  partners_cos<-paste0(getOption("baseurl"),"api/",api_version(),"/categoryOptionGroupSets/BOyWrF33hiR?fields=categoryOptionGroups[id,name,categoryOptions[id]]&paging=false") %>% 
    URLencode(.) %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("categoryOptionGroups") %>% 
    tidyr::unnest(.,cols = c(categoryOptions),names_sep=".") %>% 
    dplyr::rename(partner_name = name,
           partner_id = id,
           category_option_id = categoryOptions.id)
  
  #Return the full map of category options, category option combos, partners, agencies and mechanismsee
  dplyr::left_join(mechs,partners_cos) %>% 
    dplyr::left_join(agencies_cos)
  
  
}



d2_analyticsResponse <- function(url,remapCols=TRUE) {
print("About to get:")
  print(url)
  d<-url %>% 
    httr::GET(.) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.)
  
  if ( NROW(d$rows) > 0 ) {
    metadata <- do.call(rbind,
                        lapply(d$metaData$items,
                               data.frame, stringsAsFactors = FALSE)) %>% mutate(., from = row.names(.))
    remapMeta <-
      function(x) {
        plyr::mapvalues(x, metadata$from, metadata$name, warn_missing = FALSE)
      }
    
    d<-tibble::as_tibble(d$rows) %>% `names<-`(., d$headers$column)
    if(remapCols == TRUE) {
      d<-plyr::colwise(remapMeta)(d)
    }
    return(d) } else {
      return(NULL)
    }
}
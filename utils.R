
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


getNarrativeDataElements<-function(fiscal_year) {
  
  
   paste0(getOption("baseurl"),"api/dataElementGroups?filter=name:like:Narratives&paging=false&fields=id,name,dataElements[id,shortName]") %>% 
    httr::GET() %>% httr::content("text") %>% jsonlite::fromJSON() %>% 
    purrr::pluck("dataElementGroups") %>% 
    mutate(year =   ( stringr::str_split(name," ") ) %>% purrr::map(purrr::pluck(1)) %>% unlist() ) %>% 
    dplyr::filter(stringr::str_detect(name,"^20")) %>% 
    dplyr::mutate(type = ifelse(stringr::str_detect(name,"Result"),"Results","Targets")) %>% 
    dplyr::arrange(year,type) %>% 
    dplyr::filter(type == "Results") %>% 
    dplyr::filter(year == fiscal_year) %>% 
    purrr::pluck("dataElements") %>% 
    purrr::pluck(1) %>% 
    dplyr::arrange(shortName)
    
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
    this_quarter<-4
  } else {
    this_year<-lubridate::quarter(this_date) 
  }
  this_year
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

assemblePartnerNarrativeURL<-function(ou,fiscal_year,fiscal_quarter,selected_des,all_des) {
  
  this_period<-convertFYQuarterCalendarQuarter(fiscal_year , fiscal_quarter )
  base_url<-paste0(getOption("baseurl"),"api/analytics?")
  
  mechanisms_bit<-paste0("dimension=SH885jaRe0o")

  period_bit<-paste0("&filter=pe:", this_period)
  if(is.null(selected_des)) {
    des<-all_des
  } else {
    des<-selected_des
  }
  de_bit<-paste0("&dimension=dx:",paste(des,sep="",collapse=";"))
  ou_bit<-paste0("&filter=ou:", ou)
  end_bit<-"&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false&outputIdScheme=uid"
  paste0(base_url,mechanisms_bit,de_bit,ou_bit,period_bit,end_bit)
  
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
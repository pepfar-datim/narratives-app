
#' Title getBaseURL()
#'
#' @return A base URL to be used throughout the application. If the BASE_URL
#' environment variable is not set,
#' this function will return https://www.datim.org,
#' otherwise, the value of the environment variable.
#' @export
#'
getBaseURL <- function() {
  if (Sys.getenv("BASE_URL") !=  "")  {
    return(Sys.getenv("BASE_URL"))
  } else {
    futile.logger::flog.warn("No BASE_URL environment variable found. Using www.datim.org")
    
    return("https://www.datim.org/")
    
  }
}

api_version<-function() {"33"}

#Initiate logging
logger <- futile.logger::flog.logger()
#Load the local config file
config <- config::get()

futile.logger::flog.appender(futile.logger::appender.console(), name="narratives")

isUSGUser<-function(d2_session) {
  
    paste0(d2_session$base_url, "api/me?fields=userGroups[id,name]") %>% 
    utils::URLencode(.) %>% 
    httr::GET(.,handle=d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON() %>% 
    purrr::pluck("userGroups") %>% 
    dplyr::mutate(is_usg = stringr::str_detect(name,"Interagency|Global")) %>% 
    dplyr::pull(is_usg) %>% 
    any(.)
}

getOperatingUnits<-function(d2_session) {
  
  
  ou_map<-paste0(d2_session$base_url,"api/dataStore/dataSetAssignments/orgUnitLevels") %>% 
    URLencode(.) %>% 
    httr::GET(., handle=d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    dplyr::bind_rows() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate( country = dplyr::case_when( nchar(name4) == 0 ~ name3,
                                           TRUE ~ name4),
                   ou = name3) %>% 
    dplyr::select(ou,country)
  
  #TODO: Investigate why the API is not filtering based on the users orgunit, which really should be the case.
  ous<-paste0(d2_session$base_url,"api/organisationUnits?filter=level:lt:5&fields=id,name&filter=path:like:",d2_session$user_orgunit,"&paging=false") %>% 
    URLencode(.) %>% 
    httr::GET(., handle=d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("organisationUnits")
  
  ou_map %>% 
    dplyr::inner_join(ous,by=c("country"="name")) %>% 
    dplyr::rename(country_id = id) %>% 
    dplyr::inner_join(ous,by=c("ou"="name")) %>% 
    dplyr::rename(ou_id = id) %>% 
    dplyr::arrange(ou,country)
  
  }


getPeriods<-function() {
  
  qtrseq<-seq(as.Date("2017-01-01"), by="quarter", length.out = 30)
  qtrseq<-qtrseq[qtrseq <= Sys.Date()]
  qtrseq<-rev(paste0(as.POSIXlt(qtrseq)$year+1900, quarters(qtrseq)))
  qtrseq[2:length(qtrseq)]
}


getUSGNarrativeDataElements<-function(d2_session) {
  
  paste0(d2_session$base_url,"api/dataSets/wkdCW3M4zYT?fields=id,dataSetElements,organisationUnits") %>% 
    URLencode(.) %>% 
    httr::GET(., handle=d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("dataSetElements") %>% 
    purrr::pluck("dataElement")
  
}

getNarrativeDataElements<-function(fiscal_year, type="Results", d2_session) {
  
  
  des<- paste0(d2_session$base_url,"api/dataElementGroups?filter=name:like:Narratives&paging=false&fields=id,name,dataElements[id,shortName]") %>% 
    httr::GET(.,handle=d2_session$handle) %>% 
    httr::content("text") %>% 
    jsonlite::fromJSON() %>% 
    purrr::pluck("dataElementGroups") %>%
    tidyr::unnest_longer(.,"dataElements",simplify = TRUE) %>% 
    dplyr::mutate(de_name = dataElements$shortName, de_uid = dataElements$id) %>% 
    dplyr::select(-id,-dataElements) %>% 
    dplyr::mutate(year =   ( stringr::str_split(name," ") ) %>% purrr::map(purrr::pluck(1)) %>% unlist() ) %>% 
    dplyr::filter(stringr::str_detect(name,"^20")) %>% 
    dplyr::mutate(type = ifelse(stringr::str_detect(name,"Result"),"Results","Targets"))
  
  
  #Technical area
  
  tech_area<-paste0(d2_session$base_url,"api/dataElementGroupSets/LxhLO68FcXm?fields=dataElementGroups[id,name,dataElements[id]") %>% 
  httr::GET(.,handle=d2_session$handle) %>% httr::content("text") %>% jsonlite::fromJSON() %>% 
    purrr::pluck("dataElementGroups") %>%
    tidyr::unnest_longer(.,"dataElements",simplify = TRUE) %>% 
    dplyr::mutate( de_uid = dataElements$id) %>% 
    dplyr::select(-id,-dataElements) %>% 
    dplyr::rename("technical_area" = name )
  
  des<- des %>% dplyr::inner_join(tech_area,by="de_uid") 
  
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
    dplyr::filter(year == fiscal_year) %>% 
    dplyr::arrange(technical_area)
  
  
  
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

  fiscal_quarter<-as.integer(fiscal_quarter)

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

assemblePartnerNarrativeURL<-function(ou,fiscal_year,fiscal_quarter,all_des, d2_session ) {
  
  this_period<-convertFYQuarterCalendarQuarter(fiscal_year, fiscal_quarter )

  base_url<-paste0(d2_session$base_url,"api/analytics?")
  
  mechanisms_bit<-paste0("dimension=SH885jaRe0o")

  period_bit<-paste0("&filter=pe:", this_period)
  de_bit<-paste0("&dimension=dx:",paste(all_des,sep="",collapse=";"))
  ou_bit<-paste0("&dimension=ou:", paste(ou,sep="",collapse=";"))
  end_bit<-"&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false&outputIdScheme=uid"
  paste0(base_url,mechanisms_bit,de_bit,ou_bit,period_bit,end_bit)
  
}


assembleUSGNarrativeURL<-function(ou, fiscal_year, fiscal_quarter, d2_session ) {
  
  this_period<-convertFYQuarterCalendarQuarter(fiscal_year , fiscal_quarter)
  
  base_url<-paste0(d2_session$base_url,"api/analytics?")
  period_bit<-paste0("&filter=pe:", this_period)
  des<-getUSGNarrativeDataElements(d2_session = d2_session) %>% unlist()
  de_bit<-paste0("&dimension=dx:",paste(des,sep="",collapse=";"))
  ou_bit<-paste0("&dimension=ou:", paste(ou,sep="",collapse=";"))
  end_bit<-"&filter=ao:xYerKDKCefk&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false&outputIdScheme=uid"
  paste0(base_url,de_bit,ou_bit,period_bit,end_bit)
  
}

getUserMechanisms<-function(d2_session) {
  
  mechs<-paste0(d2_session$base_url,"api/categoryOptionCombos?filter=categoryCombo.id:eq:wUpfppgjEza&fields=id,code,name,categoryOptions[id,organisationUnits[id,name]&paging=false") %>% 
    URLencode(.) %>%
    httpcache::GET(., handle=d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.,simplifyDataFrame = TRUE) %>% 
    purrr::pluck("categoryOptionCombos") %>% 
    tidyr::unnest( cols=c("categoryOptions"), names_sep=".")
  
  #Filter out weird mechanisms with no orgunit
  mechs_no_ou <- unlist(sapply(mechs$categoryOptions.organisationUnits,function(x) NROW(x) > 0))
  mechs <- mechs[mechs_no_ou,]
  
  mechs %<>%   
    tidyr::unnest(., cols=c("categoryOptions.organisationUnits"), names_sep=".") %>% 
    dplyr::select(mech_code = code,
                  mech_name = name,
                  categoryoptioncomboid = id,
                  category_option_id = categoryOptions.id,
                  orgunit_name = categoryOptions.organisationUnits.name,
                  orgunit_id = categoryOptions.organisationUnits.id)
  

  cogs<-paste0(d2_session$base_url,"api/",api_version(),"/dimensions/SH885jaRe0o/items.json?fields=id,shortName&paging=false") %>% 
    httr::GET(., handle = d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("items")

  #Filter based on the mechs the user actually has access to
  mechs %<>%  dplyr::filter(category_option_id %in% cogs$id)
  
  #Agency map
  agencies_cos<-paste0(d2_session$base_url,"api/categoryOptionGroupSets/bw8KHXzxd9i?fields=categoryOptionGroups[id,name,categoryOptions[id]]&paging=false") %>% 
   URLencode(.) %>% 
    httr::GET(., handle = d2_session$handle) %>% 
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
  partners_cos<-paste0(d2_session$base_url,"api/categoryOptionGroupSets/BOyWrF33hiR?fields=categoryOptionGroups[id,name,categoryOptions[id]]&paging=false") %>% 
    URLencode(.) %>% 
    httr::GET(., handle = d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("categoryOptionGroups") %>% 
    tidyr::unnest(.,cols = c(categoryOptions),names_sep=".") %>% 
    dplyr::rename(partner_name = name,
           partner_id = id,
           category_option_id = categoryOptions.id)
  
  #Return the full map of category options, category option combos, partners, agencies and mechanismsee
  dplyr::inner_join(mechs,partners_cos) %>% 
    dplyr::inner_join(agencies_cos) %>% 
    dplyr::arrange(mech_code)
  
  
}

getMechDropDown<-function(mechs,ou_ids = NULL) {
  if (is.null(ou_ids)) {
    dd<-  mechs %>% 
      dplyr::select(mech_code) %>% 
      dplyr::arrange(mech_code) %>% 
      dplyr::pull(mech_code)
  } else  {
    cat(names(mechs))
    dd<-mechs %>% 
      dplyr::filter(orgunit_id %in% ou_ids) %>% 
      dplyr::select(mech_code) %>% 
      dplyr::arrange(mech_code) %>% 
      dplyr::pull(mech_code)
    
  }

}


d2_analyticsResponse <- function(url,remapCols=TRUE, d2_session) {

  d<-url %>% 
    httr::GET(., handle = d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.)
  
  if ( NROW(d$rows) > 0 ) {
    metadata <- do.call(rbind,
                        lapply(d$metaData$items,
                               data.frame, stringsAsFactors = FALSE)) %>% 
      dplyr::mutate(., from = row.names(.))
    
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


getVersionInfo <- function() {
  
  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1, "Version"]
  
  paste0("Version: ", currVersion) %>%
    paste('<div style="font-size:small;text-align: center;"><p>', .) %>%
    paste(., "</p></div>")
}
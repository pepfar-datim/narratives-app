
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


getUserOperatingUnits<-function(uid) {
  
  #Global user
  if ( uid == "ybg3MO3hcf4" ) {
    datimvalidation::getValidOperatingUnits()
  } else {
    
    datimvalidation::getValidOperatingUnits() %>% 
      dplyr::filter(id == uid)
  }
}


getUserMechanisms<-function() {
  
  paste0(getOption("baseurl"),"api/dimensions/SH885jaRe0o?fields=items[id,code]") %>% 
    httr::GET() %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON() %>% 
    purrr::pluck("items") %>% 
    dplyr::arrange(code) %>% 
    tibble::deframe()
  
}
require(datimvalidation)
require(magrittr)
require(futile.logger)
require(plyr)
require(dplyr)


source("./utils.R")

fiscal_year<-2020
fiscal_quarter<-1

loadSecrets("/home/jason/.secrets/datim-prod.json")


ous<-getOperatingUnits()
mechs<-getUserMechanisms() 
des_partner<-getNarrativeDataElements(fiscal_year)
des_usg<-getUSGNarrativeDataElements()

ous_unique<-ous %>%  dplyr::select(ou,ou_id) %>% dplyr::distinct()

for (i in 1:NROW(ous_unique)) {
  
  countries<-dplyr::filter(ous,ou_id == ous_unique$ou_id[i]) 
  
  partner_url<-assemblePartnerNarrativeURL(ou = countries$country_id,
                              fiscal_year = fiscal_year,
                              fiscal_quarter = fiscal_quarter,
                              selected_des = NULL,
                              all_des = unique(des_partner$id))

  
  is_parallel<-FALSE
  if (length(url) > 1) {
    is_parallel <- TRUE
    ncores <- parallel::detectCores() -1
    doMC::registerDoMC(cores = ncores)
  }
  
  partner_data <- plyr::llply(partner_url,d2_analyticsResponse, .parallel = is_parallel)
  
  null_filter<-lapply(partner_data, function (x) !is.null(x)) %>% unlist()
  partner_data<-partner_data[null_filter]
  partner_countries<-countries$country[null_filter]

  for (j in 1:length(partner_data)) {
    partner_data[[j]]$country <- partner_countries[j]
  }
  
  partner_data<-do.call(rbind.data.frame,partner_data)
  partner_data$ou<-ous_unique$ou[i]
  mech_codes<-stringr::str_split(partner_data$`Funding Mechanism`," - ") %>%
    purrr::map(.,purrr::pluck(2)) %>%
    unlist()
  
  partner_data$mech_code<-mech_codes
  
  partner_data %<>% dplyr::inner_join(mechs,by="mech_code") %>%
    dplyr::arrange(country,mech_code,`Data`)
  
  
  
  
  usg_url<-assembleUSGNarrativeURL(ou = countries$country_id,
                                    period = convertFYQuarterCalendarQuarter(fiscal_year,fiscal_quarter))
                                    
                                    
  usg_data<- llply(usg_url,d2_analyticsResponse, .parallel = is_parallel)
  null_filter<-lapply(usg_data, function (x) length(x)) %>% unlist() > 0
  usg_data<-usg_data[null_filter]
  usg_countries<-countries$country[null_filter]
  
  
  for (j in 1:length(usg_data)) {
    usg_data[[j]]$country <- usg_countries[j]
  }
  
  usg_data<-do.call(rbind.data.frame,usg_data)
  usg_data$ou<-ous_unique$ou[i]
  
  
  tryCatch(rmarkdown::render(
    paste0( "partner_narratives_template.Rmd"),
    output_file = paste0( ous_unique$ou[i], ".pdf")
  ),
  error = function(e) {
    print(paste("Could not render report for ", ous_unique$ou[i]))
  } )
  

  }
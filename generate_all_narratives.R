
require(magrittr)
require(futile.logger)
require(plyr)
require(dplyr)
require(datimutils)


source("./utils.R")

fiscal_year<-2023
fiscal_quarter<-1

input<-list(fiscal_year=fiscal_year,fiscal_quarter=fiscal_quarter)

loginToDATIM("/home/jason/.secrets/datim.json")

#Initial setup
ous<-getOperatingUnits(d2_session = d2_default_session)
mechs<-getUserMechanisms(d2_session = d2_default_session) 
des_partner<-getNarrativeDataElements(fiscal_year, d2_session = d2_default_session)
des_usg<-getUSGNarrativeDataElements(d2_session = d2_default_session)

ous_unique<-ous %>%  dplyr::select(ou,ou_id) %>% dplyr::distinct()

for (i in 1:NROW(ous_unique)) {
  
  countries<-dplyr::filter(ous,ou_id == ous_unique$ou_id[i]) 
  
  partner_url<-assemblePartnerNarrativeURL(ou = countries$country_id,
                              fiscal_year = fiscal_year,
                              fiscal_quarter = fiscal_quarter,
                              all_des = unique(des_partner$de_uid),
                              d2_session = d2_default_session)

  partner_data <- d2_analyticsResponse(partner_url, d2_session = d2_default_session) %>% 
    dplyr::rename(country = `Organisation unit`) %>% 
    dplyr::mutate(mech_code = (stringr::str_split(`Funding Mechanism`," - ") %>%
                                 purrr::map(.,purrr::pluck(2)) %>%
                                 unlist()) ) %>% 
    dplyr::inner_join(mechs,by="mech_code") %>% 
    dplyr::left_join(des_partner, by=c(`Data` = "de_name")) %>% 
    dplyr::rename(ou = orgunit_name)%>% 
    dplyr::arrange(country,mech_code,technical_area,support_type)

    
  usg_data<-assembleUSGNarrativeURL(ou = countries$country_id,
                                    fiscal_year,
                                   fiscal_quarter ) %>% 
    d2_analyticsResponse(.) %>% 
    dplyr::rename(country = `Organisation unit`) %>%
    dplyr::mutate(ou =  ous_unique$ou[i] ) %>% 
    dplyr::left_join(des_partner,by=c(`Data` = "de_name")) %>% 
    dplyr::arrange(country,technical_area,support_type)
  
  d<-list(partners=partner_data,usg=usg_data)
  
  tryCatch(rmarkdown::render(
    paste0( "partner_narratives_template.Rmd"),
    output_file = paste0( ous_unique$ou[i], ".pdf")
  ),
  error = function(e) {
    print(paste("Could not render report for ", ous_unique$ou[i]))
  } )
  

  }
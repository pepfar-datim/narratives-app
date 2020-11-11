library(shiny)
library(shinyjs)
library(shinyWidgets)
require(DT)
require(digest)

require(waiter)


source("./utils.R")

shinyServer(function(input, output, session) {
  
  
  ready <- reactiveValues(ok = FALSE,
                          needs_refresh = TRUE)
  
  user_input <- reactiveValues(authenticated = FALSE,
                               username = NA,
                               authorization_header=NA,
                               fiscal_year = getCurrentFiscalYear(),
                               fiscal_quarter = getCurrentFiscalQuarter(),
                               is_usg_user = FALSE,
                               is_global_user = FALSE,
                               user_operating_units=NA,
                               operating_units_dropdown=NA,
                               user_mechs=NA,
                               mech_dropdown = NA,
                               partner_data_elements=NA,
                               data_elements_dropdown=NULL,
                               selected_data_elements = NULL,
                               selected_mechanisms = NULL,
                               has_des_filter = FALSE)
  
  selected_ous<-reactiveValues(selected_ous = NULL)
  
  observeEvent(input$fetch, {
    ready$ok <- TRUE
    ready$needs_refresh <- TRUE
  })  
  
  
  #Observers for UI
  observeEvent(input$reset_input, {

    shinyjs::reset("side-panel")
    shinyjs::reset("des")
    shinyjs::reset("mechs")
    shinyjs::reset("fiscal_year")
    shinyjs::reset("ou")
    user_input$selected_data_elements<-NULL
    user_input$selected_mechanisms<-NULL
    ready$ok <- FALSE
    ready$needs_refresh <- FALSE

  })
  
  observeEvent(input$fiscal_year, {
    user_input$fiscal_year <-input$fiscal_year
  })
  
  observeEvent(input$fiscal_quarter, {
    user_input$fiscal_quarter <- input$fiscal_quarter
  })
  
  observeEvent(input$mechs, {
    user_input$selected_mechs <-input$mechs
  })
  
  observeEvent(input$ou,{

    dd<-getMechDropDown(user_input$user_mechs,input$ou)
    
    updateSelectizeInput(session=session,
                         inputId = "mechs",
                         selected = NULL, choices = dd)
  }, ignoreNULL = FALSE,ignoreInit = TRUE)
  
  
  observeEvent(input$des, {
    user_input$has_des_filter<-TRUE
    user_input$selected_data_elements <-input$des
  })
  
  observeEvent(input$login_button, {
    is_logged_in <- FALSE

    login_status <- DHISLogin(config$baseurl, input$user_name,input$password)
    
    user_input$authenticated<-login_status$status
    user_input$httr_handle<-login_status$handle
    user_input$user_operating_unit<-login_status$user_org_unit
    user_input$username<-input$user_name

    if (user_input$authenticated) {

      waiter_show(html = waiting_screen, color = "black")

      flog.info(paste0("User ", input$user_name, " logged in."), name = "datapack")
      user_input$user_operating_units <- getOperatingUnits(handle=user_input$httr_handle)
      
      user_input$operating_units_dropdown <- user_input$user_operating_units %>% 
        dplyr::select(ou,ou_id) %>% 
        tibble::deframe()
      
      user_input$is_global_user <- user_input$user_operating_unit == "ybg3MO3hcf4"
      
      user_input$is_usg_user <- isUSGUser(handle = user_input$httr_handle)
      
      user_input$user_mechs<-getUserMechanisms(handle = user_input$httr_handle) 
      
      user_input$mech_dropdown <- getMechDropDown(user_input$user_mechs,NULL)
      
      user_input$partner_data_elements<-getNarrativeDataElements(user_input$fiscal_year, handle =  user_input$httr_handle)
      
      user_input$data_elements_dropdown <- user_input$partner_data_elements %>% 
        dplyr::select(technical_area) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(technical_area)

      flog.info(paste0("User operating unit is ", user_input$user_operating_unit))

      waiter_hide()
      
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error")
      flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
    }
  })  
  
  
  #UI section
  output$ui <- renderUI({
    
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        use_waiter(),
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      
      
      fluidPage(
        tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
        use_waiter(),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            id = "side-panel",
            tags$hr(),
            selectInput(inputId = "ou", 
                        label= "Operating Unit",
                        user_input$operating_units_dropdown,
                        multiple = TRUE,
                        selected = ifelse(user_input$user_operating_units == "ybg3MO3hcf4",NULL,user_input$user_operating_units)),
            tags$hr(),
            selectInput(inputId = "fiscal_year", 
                        label= "Fiscal Year",
                        c("FY20"=2020,"FY19"=2019,"FY18"=2018,",FY17"=2017,"FY16"=2016)),
            tags$hr(),
            selectInput(inputId = "fiscal_quarter", 
                        label= "Fiscal Quarter",
                        c(1,2,3,4),
                        selected=getCurrentFiscalQuarter()),
            tags$hr(),
            selectizeInput(inputId = "mechs", 
                        label= "Mechanisms",
                        choices = user_input$mech_dropdown,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = 'Select one or more mechanisms:')),
            tags$hr(),
            selectizeInput(inputId = "des",
                        label = "Technical areas",
                        choices = user_input$data_elements_dropdown,
                        multiple = TRUE,
                        selected = NULL,
                        
                        options = list(placeholder = 'Select technical areas:', maxItems = 5)),
            tags$hr(),
            textInput(inputId = "free_text_filter",
                               label = "Search:",
                               placeholder = "Free text search:"),
            tags$hr(),
            conditionalPanel(checkboxInput("includeUSGNarratives",label = "Include USG Narratives"),condition = user_input$is_usg_user),
            tags$hr(),
            actionButton("fetch","Get Narratives"),
            tags$hr(),
            actionButton("reset_input","Reset choices"),
            tags$hr(),
            disabled(downloadButton('downloadReport',"Download PDF")),
            tags$hr(),
            disabled(downloadButton('downloadXLSX','Download XLSX')),
            tags$hr(),
            disabled(downloadButton('downloadDocx','Download DOCX'))
          ),
          mainPanel(tabsetPanel(
            id = "main-panel",
            type = "tabs",
            tabPanel("Narratives", dataTableOutput('narratives')),
            tabPanel("USG Narratives",dataTableOutput('usg_narratives'))
            
          ))
        ))
  }
})
  
  waiting_screen <- tagList(
    spin_solar(),
    h4("Getting things set up. Please wait...")
  ) 
  
  waiting_screen_pdf <- tagList(
    spin_hourglass(),
    h4("Producing a PDF of the selected narratives. Please wait...")
  ) 

  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    
    wellPanel(fluidRow(
      img(src='pepfar.png', align = "center"),
      h4("Welcome to the Results Narratives App. Please login with your DATIM credentials:")
    ),
    fluidRow(
      textInput("user_name", "Username: ",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })
  
  
  #Outputs 
  output$narratives <- DT::renderDataTable({
    
    vr<-filtered_narratives()

    if (!inherits(vr,"error") & !is.null(vr$partner) ){
     vr %>% 
        purrr::pluck("partner") %>% 
        dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Mechanism" = mech_code,
                      "Agency" = agency_name,
                      "Partner" = partner_name,
                      "Technical area" = technical_area,
                      "Support type" = support_type,
                      "Narrative" = `Value`) %>%
        dplyr::arrange(`Operating unit`,`Country`,Partner,Mechanism,`Technical area`)
        
    } else {
      data.frame("Message" = "No records found. Try a different combination of paramaters.")
    }
  },options=list(
    bFilter=0,
    bInfo=0,
    columnDefs = list(list(
      targets = c(8),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 100 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
        "}")))))
  
  output$usg_narratives <- DT::renderDataTable({
    
    vr<-filtered_narratives()

    if (!inherits(vr,"error") & !is.null(vr$usg)){
      
      vr %<>%  purrr::pluck("usg")
      
      if ( is.null(vr) | NROW(vr) == 0) {
        return(data.frame("Message" = "No records found. Try a different combination of paramaters."))
      } else {
        vr %<>% 
        dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Technical area" = technical_area,
                      "Support type" = support_type,
                      "Narrative" = `Value`) %>%
          dplyr::arrange(`Operating unit`,`Country`,`Technical area`)
        return(vr)
      }
    } else {
      data.frame("Message" = "No records found. Try a different combination of paramaters.")
    }
  },options=list(
    bFilter=0,
    bInfo=0,
    columnDefs = list(list(
      targets = c(5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 100 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
        "}")))))
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      
      paste0('narrative-report', '.', 'pdf')
    },
    
    content = function(file) {
      
      waiter_show(html = waiting_screen_pdf, color = "rgba(128,128,128,.8)" )
      src <- normalizePath('partner_narratives_template.Rmd')
      img <- normalizePath('pepfar.png')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(img, 'pepfar.png', overwrite = TRUE)
      flog.info(paste0("User ", user_input$username, " requested a PDF output."), name = "narratives")
      
      library(rmarkdown)
      out <- rmarkdown::render('report.Rmd', pdf_document(latex_engine = "xelatex"))
      waiter_hide()  
      file.rename(out, file)
    }
  )
  
  
  output$downloadDocx <- downloadHandler(
    filename = function() {
      
      paste0('narrative-report', '.', 'docx')
    },
    
    content = function(file) {
      
      flog.info(paste0("User ", user_input$username, " requested a DOCX output."), name = "narratives")
      src <- normalizePath('partner_narratives_template.Rmd')
      img <- normalizePath('pepfar.png')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(img, 'pepfar.png', overwrite = TRUE)
      
      library(rmarkdown)
      
      out <- rmarkdown::render('report.Rmd', word_document())
      file.rename(out, file)
    }
  )
  
  
  output$downloadXLSX <- downloadHandler(
    filename = function() {
      
      paste0('narrative-report', '.', 'xlsx')
    },
    
    content = function(file) {
      flog.info(paste0("User ", user_input$username, " requested a XLSX output."), name = "narratives")
      vr<-list()
      
      partner_data<-filtered_narratives() %>% 
        purrr::pluck("partner") 
      if (!is.null(partner_data) & NROW(partner_data) > 0) {
        partner_data %<>% dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Mechanism" = mech_code,
                      "Agency" = agency_name,
                      "Partner" = partner_name,
                      "Technical area" = technical_area,
                      "Support type" = support_type,
                      "Narrative" = `Value`) %>%
          dplyr::arrange(`Operating unit`,Country,Partner,Mechanism,`Technical area`)
        vr$partner_data<-partner_data
      }

    
      usg_data<-filtered_narratives() %>% 
        purrr::pluck("usg") 
      
      if (!is.null(usg_data) & NROW(usg_data) > 0 ){
        
        usg_data %<>% dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Technical area" = technical_area,
                      "Support type" = support_type,
                      "Narrative" = `Value`) 
        vr$USG<-usg_data
      }
      
      
      openxlsx::write.xlsx(vr, file = file)
    }
  )
  
  fetch<-function() {
    
    if (!ready$ok) {
      return(NULL)
    } else {
      
      
      #Select all countries if none are explicitly selected
      countries<-
        if (is.null(input$ou)) {
          user_input$user_operating_units %>% 
            pull(country_id)
        } else {
          dplyr::filter(user_input$user_operating_units
                        ,ou_id %in% input$ou) %>% 
            dplyr::pull(country_id)
        }
        

      
      get_des<- function()
        {
            if ( is.null(input$des )) {return(user_input$partner_data_elements$de_uid)} else {
              user_input$partner_data_elements %>% 
                dplyr::filter(technical_area %in% input$des) %>% 
                dplyr::pull(de_uid) %>% 
                unique(.)
            } }
        
      
      d<-list()
      
      url <- assemblePartnerNarrativeURL(ou = countries, 
                                         fiscal_year = user_input$fiscal_year,
                                         fiscal_quarter = user_input$fiscal_quarter,
                                         all_des = get_des())

  
      d$partner <- d2_analyticsResponse(url, handle = user_input$httr_handle)
    
      if (user_input$is_usg_user  & input$includeUSGNarratives ) {
        url<- assembleUSGNarrativeURL(ou = countries,
                                      fiscal_year = user_input$fiscal_year,
                                      fiscal_quarter = user_input$fiscal_quarter,
                                      handle = user_input$httr_handle)
 
        d$usg<-d2_analyticsResponse(url, handle = user_input$httr_handle)
     
      } else {
        d$usg<-NULL
      }

      
      #Enable the button and return the data
      
      if (all(is.null(d)) ) {
        
        shinyjs::disable("downloadReport")
        shinyjs::disable("downloadXLSX")
        shinyjs::disable("downloadDocx")
        shinyjs::enable("fetch")
        
        return(NULL)
        ready$needs_refresh <- FALSE
      }
      
      if(!is.null(d$partner)) {
        
        d$partner  %<>% dplyr::rename(country = `Organisation unit`) %>% 
          dplyr::inner_join(user_input$user_operating_units,by="country") %>% 
          dplyr::mutate(mech_code =  ( stringr::str_split(`Funding Mechanism`," - ") %>% 
                                         map(.,purrr::pluck(2)) %>% 
                                         unlist() ) ) %>% 
          dplyr::left_join(user_input$user_mechs, by = "mech_code") %>%  
          dplyr::left_join(user_input$partner_data_elements, by=c(`Data` = "de_name"))
      }

      
      if (!is.null(d$usg)) {
        
        d$usg %<>% dplyr::rename(country = `Organisation unit`) %>% 
          dplyr::inner_join(user_input$user_operating_units,by="country") %>% 
          dplyr::left_join(user_input$partner_data_elements, by=c(`Data` = "de_name"))
      }
      
      
      ready$needs_refresh <- FALSE

      shinyjs::enable("downloadReport")
      shinyjs::enable("downloadXLSX")
      shinyjs::enable("downloadDocx")
      shinyjs::enable("fetch")
    } 
    
    d
  }
  
  narrative_results <- reactive({
    
    
    if (input$fetch == 0) { return(NULL) }
    
    isolate({ 
      
    needs_des_filter<- function() {
      
      if ( is.null(input$ou) | length(input$ou) > 1 ) {
      
      if ( is.null(input$des) ) { return(TRUE) }
      if ( length(input$des) < 6 ) {return(FALSE)}
      
    } else {
      return(FALSE)
    }}
    
    if (needs_des_filter()) {
      sendSweetAlert(
        session,
        title = "Please add some filters",
        text = "You have selected multiple operating units. Please select between 1 and 5 technical areas.",
        type = "error")
      return(NULL)
    }
    
 fetch()  })}
    
)
  
  
  filtered_narratives <- reactive({
    
    d <- narrative_results ()
    
    if (all(is.null(d))) {
      return(NULL)
    }

    if (!input$includeUSGNarratives) {d$usg<-NULL}
    
    if (input$free_text_filter != "") {
      

      row_filter<- function(df, filter_string)  {
        
        if (is.null(df)) {return(NULL)}
        keep_rows<-df %>% dplyr::rowwise() %>% 
        purrr::map_dfr(.,function(x) stringr::str_detect(x,filter_string)) %>% 
        rowSums(.) %>% 
        as.logical(.)
        
        df[keep_rows,] }
        
      d<-lapply(d,function(x) row_filter(x,input$free_text_filter))

    
    }
    
    if (!is.null(input$des) & !is.null(d$usg)) {
      d$usg<-d$usg %>% dplyr::filter(technical_area %in% input$des)
    }
    
    if (!is.null(input$mechs) & is.null(d$partner)) {
      
      d$partner<-d$partner %>%  dplyr::filter(mech_code %in% input$mechs)
      
    }
    
    

    d 
    
  })
  
  })

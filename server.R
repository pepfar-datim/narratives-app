
library(shiny)
library(shinyjs)
library(shinyWidgets)
require(magrittr)
require(dplyr)
require(datimvalidation)
require(ggplot2)
require(futile.logger)
require(scales)
require(DT)
require(config)
require(purrr)
require(praise)
require(scales)
require(plyr)
require(doMC)
require(rpivotTable)
require(waiter)

source("./utils.R")

shinyServer(function(input, output, session) {
  
  
  ready <- reactiveValues(ok = FALSE,
                          needs_refresh = TRUE)
  
  user_input <- reactiveValues(authenticated = FALSE, 
                               fiscal_year = getCurrentFiscalYear(),
                               fiscal_quarter = getCurrentFiscalQuarter(),
                               user_operating_units=NA,
                               operating_units_dropdown=NA,
                               user_mechs=NA,
                               mech_dropdown = NA,
                               partner_data_elements=NA,
                               data_elements_dropdown=NULL,
                               selected_data_elements = NULL,
                               selected_mechanisms = NULL,
                               has_des_filter = FALSE)
  
  observeEvent(input$fetch, {
    ready$ok <- TRUE
    ready$needs_refresh <- TRUE
  })  
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::reset("des")
    shinyjs::reset("mechs")
    shinyjs::reset("fiscal_year")
    shinyjs::reset("ou")
    shinyjs::disable("fetch")

  })
  
  observeEvent(input$fiscal_year, {
    user_input$fiscal_year <-input$fiscal_year
  })
  
  observeEvent(input$fiscal_quarter, {
    user_input$fiscal_quarter <- input$fiscal_quarter
  })
  
  observeEvent(input$mechs, {
    user_input$selected_mechs <-input$mechs
    print(user_input$selected_mechs)
  })
  
  
  observeEvent(input$des, {
    user_input$has_des_filter<-TRUE
    user_input$selected_data_elements <-input$des
    print("Data elements which are selected: ")
    print(user_input$selected_data_elements)
  })
  
  observeEvent(input$login_button, {
    is_logged_in <- FALSE
    user_input$authenticated <- DHISLogin(input$server, input$user_name, input$password)
    if (user_input$authenticated) {

      waiter_show(html = waiting_screen, color = "black")

      
      #user_input$user_orgunit<-getOption("organisationUnit")
      flog.info(paste0("User ", input$user_name, " logged in."), name = "datapack")
      user_input$user_operating_units <- getOperatingUnits() 
      
      user_input$operating_units_dropdown <- user_input$user_operating_units %>% 
        dplyr::select(ou,ou_id) %>% 
        tibble::deframe()
      
      user_input$user_mechs<-getUserMechanisms() 
      
      user_input$mech_dropdown <- user_input$user_mechs %>% 
        dplyr::select(mech_code) %>% 
        dplyr::arrange(mech_code)
      
      user_input$partner_data_elements<-getNarrativeDataElements(user_input$fiscal_year)
      
      user_input$data_elements_dropdown <- user_input$partner_data_elements %>% 
        dplyr::select(technical_area) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(technical_area)

      flog.info(paste0("User operating unit is ", getOption("organisationUnit")))

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
                        user_input$operating_units_dropdown),
            tags$hr(),
            selectInput(inputId = "fiscal_year", 
                        label= "Fiscal Year",
                        c("FY20"=2020,"FY19"=2019,"FY18"=2018,"FY17"=2017,"FY16"=2016)),
            tags$hr(),
            selectInput(inputId = "fiscal_quarter", 
                        label= "Fiscal Quarter",
                        c(1,2,3,4)),
            tags$hr(),
            selectizeInput(inputId = "mechs", 
                        label= "Mechanisms",
                        choices = user_input$mech_dropdown,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = 'Select one or more mechanisms:')),
            tags$hr(),
            selectizeInput(inputId = "des",
                        label = "Data elements",
                        choices = user_input$data_elements_dropdown,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = 'Select one or more narratives:')),
            tags$hr(),
            textInput(inputId = "free_text_filter",
                               label = "Search:",
                               placeholder = "Free text search including regex"),
            tags$hr(),
            actionButton("fetch","Get Narratives"),
            tags$hr(),
            actionButton("reset-input","Reset choices"),
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
            tabPanel("Narratives", dataTableOutput('narratives'))
            
          ))
        ))
  }
})
  
  waiting_screen <- tagList(
    spin_solar(),
    h4("Getting things set up. Please wait...")
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

    
    if (!inherits(vr,"error") & !is.null(vr)){
      vr %>% 
        dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Mechanism" = mech_code,
                      "Agency" = agency_name,
                      "Partner" = partner_name,
                      "Technical area" = technical_area,
                      "Support type" = support_type,
                      "Narrative" = `Value`) %>%
        dplyr::arrange(Partner,Mechanism,`Technical area`)
        
    } else {
      print("VR is null!!!  ")
      NULL
    }
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      
      paste('narrative-report', '.', 'pdf')
    },
    
    content = function(file) {
      
      
      src <- normalizePath('partner_narratives_template.Rmd')
      img <- normalizePath('pepfar.png')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(img, 'pepfar.png', overwrite = TRUE)
      
      library(rmarkdown)
      out <- rmarkdown::render('report.Rmd', pdf_document(latex_engine = "xelatex"))
      file.rename(out, file)
    }
  )
  
  
  output$downloadDocx <- downloadHandler(
    filename = function() {
      
      paste0('narrative-report', '.', 'docx')
    },
    
    content = function(file) {
      
      
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
      vr<-filtered_narratives() %>% 
        dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Mechanism" = mech_code,
                      "Agency" = agency_name,
                      "Partner" = partner_name,
                      "Technical area" = technical_area,
                      "Support type" = support_type,
                      "Narrative" = `Value`) %>%
        dplyr::arrange(Country,Partner,Mechanism,`Technical area`)
      openxlsx::write.xlsx(vr, file = file)
    }
  )
  
  fetch<-function() {
    
    if (!ready$ok) {
      return(NULL)
    } else {
      
      countries<-dplyr::filter(user_input$user_operating_units
                               ,ou_id == input$ou) %>% 
                 dplyr::pull(country_id)
      
      
      url <- assemblePartnerNarrativeURL(ou = countries, 
                                         fiscal_year = user_input$fiscal_year,
                                         fiscal_quarter = user_input$fiscal_quarter,
                                         all_des = user_input$partner_data_elements$de_uid)
      is_parallel<-FALSE
      # if (length(url) > 1) {
      #   is_parallel <- TRUE
      #   ncores <- parallel::detectCores() -1
      #   doMC::registerDoMC(cores = ncores)
      # }

      d <- llply(url,d2_analyticsResponse, .parallel = is_parallel)
      d<-setNames(d,countries)
      d_is_not_null<-lapply(d,function(x) !is.null(x) ) %>% unlist()
      d<-d[d_is_not_null]
      
     
      
      #Enable the button and return the data
      
      if (NROW(d) == 0 ) {
        
        
        shinyjs::disable("downloadReport")
        shinyjs::disable("downloadXLSX")
        shinyjs::disable("downloadDocx")
        
        shinyjs::enable("fetch")
  
        return(tibble::tibble(
          "ou",
          "country",
          "mech_code",
          "agency_name",
          "partner_name",
          'Data',
          'technical_area',
          'support_type',
          'Value'
        ))
        ready$needs_refresh <- FALSE
      }
      
      d<-tibble::enframe(d) %>% 
        tidyr::unnest(cols=c(value)) %>% 
        dplyr::rename(country_id = name) %>% 
        dplyr::inner_join(user_input$user_operating_units,by="country_id") %>% 
      dplyr::mutate(mech_code =  ( stringr::str_split(`Funding Mechanism`," - ") %>% 
                 map(.,purrr::pluck(2)) %>% 
                 unlist() ) ) %>% 
        dplyr::left_join(user_input$user_mechs, by = "mech_code") %>%  
      dplyr::left_join(user_input$partner_data_elements, by=c(`Data` = "de_name"))
      
      ready$needs_refresh <- FALSE

      shinyjs::enable("downloadReport")
      shinyjs::enable("downloadXLSX")
      shinyjs::enable("downloadDocx")
      shinyjs::enable("fetch")
    } 
    
    d
  }
  
  narrative_results <- reactive({ fetch() })
  
  filtered_narratives <- reactive({
    
    d <- narrative_results ()

    if (!is.null(input$des)) {
      
       d %<>% dplyr::filter(technical_area %in% input$des)
     }
    
    if (!is.null(input$mechs)) {
      d %<>% dplyr::filter(mech_code %in% input$mechs)
    }
    
    if (input$free_text_filter != "") {
      row_filter<- d %>% dplyr::rowwise() %>% 
        purrr::map_dfr(.,function(x) stringr::str_detect(x,input$free_text_filter)) %>% 
        rowSums(.) %>% 
        as.logical(.)
      
      d<-d[row_filter,]
    
      }
    
    d
    
  })
  
  })

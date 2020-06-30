
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
  
  w <- Waiter$new()
  
  ready <- reactiveValues(ok = FALSE)
  
  user_input <- reactiveValues(authenticated = FALSE, 
                               status = "",
                               user_operating_units=NA,
                               operating_units_dropdown=NA,
                               user_mechs=NA,
                               mech_dropdown = NA)
  
  observeEvent(input$fetch, {
    shinyjs::disable("fetch")
    ready$ok <- TRUE
  })  
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::disable("fetch")
    ready$ok<-FALSE
  })
  
 
  
  observeEvent(input$login_button, {
    is_logged_in <- FALSE
    user_input$authenticated <- DHISLogin(input$server, input$user_name, input$password)
    if (user_input$authenticated) {

      w$show()
      
      #user_input$user_orgunit<-getOption("organisationUnit")
      flog.info(paste0("User ", input$user_name, " logged in."), name = "datapack")
      user_input$user_operating_units <- getOperatingUnits() 
      
      user_input$operating_units_dropdown <- user_input$user_operating_units %>% 
        dplyr::select(ou,ou_id) %>% 
        tibble::deframe()
      
      user_input$user_mechs<-getUserMechanisms() 
      
      user_input$mech_dropdown <- user_input$user_mechs %>% 
        dplyr::select(mech_code,categoryoptioncomboid) %>% 
        tibble::deframe()
      
      flog.info(paste0("User operating unit is ", getOption("organisationUnit")))

      w$hide()
      
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
            selectInput(inputId = "mechs", 
                        label= "Mechanisms",
                        user_input$mech_dropdown ),
            tags$hr(),
            actionButton("fetch","Get Narratives"),
            tags$hr(),
            disabled(downloadButton('downloadReport',"Download PDF")),
            disabled(downloadButton('downloadXLSX','Download XLSX'))
          ),
          mainPanel(tabsetPanel(
            id = "main-panel",
            type = "tabs",
            tabPanel("Narratives", dataTableOutput('narratives'))
            
          ))
        ))
  }
})
  

  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    
    wellPanel(fluidRow(
      img(src='pepfar.png', align = "center"),
      h4("Welcome to the Combined Narratives App. Please login with your DATIM credentials:")
    ),
    fluidRow(
      textInput("user_name", "Username: ",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })
  
  
  #Outputs 
  output$narratives <- DT::renderDataTable({
    
    vr<-narrative_results()  

    
    if (!inherits(vr,"error") & !is.null(vr)){
      vr %>% 
        dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Mechanism" = mech_code,
                      "Agency" = agency_name,
                      "Partner" = partner_name,
                      "Technical area" = `Data`,
                      "Narrative" = `Value`) %>%
        dplyr::arrange(Partner,Mechanism,`Technical area`)
        
    } else {
      NULL
    }
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      
      paste('narrative-report', '.', 'pdf')
    },
    
    content = function(file) {
      
      
      src <- normalizePath('partner_narratives_template.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- rmarkdown::render('report.Rmd', pdf_document(latex_engine = "xelatex"))
      file.rename(out, file)
    }
  )
  
  
  output$downloadXLSX <- downloadHandler(
    filename = function() {
      
      paste0('narrative-report', '.', 'xlsx')
    },
    
    content = function(file) {
      vr<-narrative_results() %>% 
        dplyr::select("Operating unit"  = ou,
                      "Country" = country,
                      "Mechanism" = mech_code,
                      "Agency" = agency_name,
                      "Partner" = partner_name,
                      "Technical area" = `Data`,
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
                                         fiscal_year = input$fiscal_year,
                                         fiscal_quarter = input$fiscal_quarter)
      
  
      is_parallel<-FALSE
      if (length(url) > 1) {
        is_parallel <- TRUE
        ncores <- parallel::detectCores() -1
        doMC::registerDoMC(cores = ncores)
      }

      d <- llply(url,d2_analyticsResponse, .parallel = is_parallel)
      d<-setNames(d,countries)
      d_is_not_null<-lapply(d,function(x) !is.null(x) ) %>% unlist()
      d<-d[d_is_not_null]
      #Enable the button and return the data
      
      d<-tibble::enframe(d) %>% 
        tidyr::unnest(cols=c(value)) %>% 
        dplyr::rename(country_id = name) %>% 
        dplyr::inner_join(user_input$user_operating_units,by="country_id") %>% 
      dplyr::mutate(mech_code =  ( stringr::str_split(`Funding Mechanism`," - ") %>% 
                 map(.,purrr::pluck(2)) %>% 
                 unlist() ) ) %>% 
        dplyr::left_join(user_input$user_mechs, by = "mech_code")
      shinyjs::enable("downloadReport")
      shinyjs::enable("downloadXLSX")
      shinyjs::enable("fetch")
    } 
    
    d
  }
  
  narrative_results <- reactive({ fetch() })
  
  })

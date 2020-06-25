
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
require(rpivotTable)

source("./utils.R")

shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  user_input <- reactiveValues(authenticated = FALSE, 
                               status = "",
                               user_operating_units=NA,
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
      #user_input$user_orgunit<-getOption("organisationUnit")
      flog.info(paste0("User ", input$user_name, " logged in."), name = "datapack")
      user_input$user_operating_units<- getUserOperatingUnits(getOption("organisationUnit")) %>% 
        dplyr::select(name,id) %>% 
        tibble::deframe()
      
      user_input$user_mechs<-getUserMechanisms() 
      
      user_input$mech_dropdown <- user_input$user_mechs %>% 
        dplyr::select(mech_code,categoryoptioncomboid) %>% 
        tibble::deframe()
      
      flog.info(paste0("User operating unit is ", getOption("organisationUnit")))

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
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            id = "side-panel",
            tags$hr(),
            selectInput(inputId = "ou", 
                        label= "Operating Unit",
                        user_input$user_operating_units),
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
            actionButton("fetch","Fetch"),
            radioButtons('format', 'Document format', c('PDF', 'XLSX'),
                         inline = TRUE),
            downloadButton('downloadReport')
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
      vr
    } else {
      NULL
    }
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', XLSX = 'xlsx'
      ))
    },
    
    content = function(file) {
      
      
      src <- normalizePath('partner_narratives_template.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(latex_engine = "xelatex"), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  fetch<-function() {
    
    
    if (!ready$ok) {
      return(NULL)
    } else {
      

      url <- assemblePartnerNarrativeURL(ou = input$ou, 
                                         fiscal_year = input$fiscal_year,
                                         fiscal_quarter = input$fiscal_quarter)
      
      
      d <- d2_analyticsResponse(url) %>%
        dplyr::arrange(`Funding Mechanism`) %>%
        dplyr::arrange(`Data`) %>%
        dplyr::mutate(mech_code =  {
          stringr::str_split(`Funding Mechanism`, " - ") %>%
            map(., purrr::pluck(2)) %>% 
            unlist()
        } ) %>% 
          dplyr::left_join(user_input$user_mechs, by = "mech_code")
      return(d)
    } 
    
  }
  
  narrative_results <- reactive({ fetch() })
  
  })

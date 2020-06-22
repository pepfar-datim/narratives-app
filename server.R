
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
source("./partner_narratives.R")

shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  user_input <- reactiveValues(authenticated = FALSE, 
                               status = "",
                               user_operating_units=NA,
                               user_mechs=NA)
  
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
      
      flog.info(paste0("User operating unit is ", getOption("organisationUnit")))
      print(user_input$user_operating_unit)
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
            selectInput(inputId = "mechs", 
                        label= "Mechanisms",
                        user_input$user_mechs),
            actionButton("fetch","Fetch")
          ),
          mainPanel(tabsetPanel(
            id = "main-panel",
            type = "tabs"
            
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
  
  fetch<-function() {
    
    
    if (!ready$ok) {
      shinyjs::disable("fetch")
      return(NULL)
    }
    
    messages<-""
  
    messages<-list()
    d<-NULL
    return(d)
    
  }
  
  narrative_results <- reactive({ fetch() })
  
  })

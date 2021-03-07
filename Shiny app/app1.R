library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(rdrop2)
drop_auth()

source("helpers.R") # Load all the code needed to show feedback on a button click

#data <- read.csv("savedrecs.csv")
#write.csv(data,"selection_output.csv")
data<-read.csv("https://www.dropbox.com/s/serwffb516rv843/selection_output.csv?dl=1", row.names = NULL)
#data<-read.csv("selection_output.csv")
data_col <- colnames(data)
if(!"FirstReview" %in% data_col){
  data$FirstReview <- NA
  data$SecondReview <- NA
  data$FirstReviewer <- NA
  data$SecondReviewer <- NA
  write.csv(data, "selection_output.csv")
}

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Forecast Screening"),
                title = "Forecast Screening",
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.submit_name == true",
                    textOutput("name"),
                    h3("You have completed"),
                    h1(textOutput("num")),
                    h3("screenings"),
                    hr(),
                    textOutput("total")
                    )),
                
                # Output: 
                mainPanel(
                  h2("You have been assigned this paper:"),
                  textOutput("paper_name"),
                  h3("Abstract"),
                  htmlOutput("abstract"),
                  htmlOutput("link"),
                  h2("Should we use it in our analysis?"),
                  selectInput("assessment",label = NULL,choices = c("Select answer"=-99,"Yes!"=1,"No"=0,"Unsure"=99)),
                  withBusyIndicatorUI(actionButton("submit","submit")),
                  br(),br(),br(),br(),br(),br(),br(),br()
                )
)

#Define server function
server <- function(input, output, session) {
  if(sum(is.na(data$FirstReview))>0){
    num = sample(seq(1,nrow(data), by = 1)[is.na(data$FirstReview)],1)
  } else{
    num = sample(seq(1,nrow(data), by = 1)[is.na(data$SecondReview)&data$FirstReviewer!=tolower(trimws(input$name))],1)
  }
  paper_num <- reactiveVal(value = num)
  
  nameModal <- function(real = TRUE) {
    modalDialog(
      span('What is your name?'),
      textInput(inputId = "name", label = " ", placeholder = "First Last"),
      if(!real){
        div(tags$b("Invalid name", style = "color: red;"))
      },
      footer = tagList(
        actionButton("submit_name", "submit")
      )
    )
  }
  
  showModal(nameModal())
  
  observeEvent(input$submit_name, handlerExpr = {
    if(nchar(input$name)==0){
      showModal(nameModal(real = FALSE))
    }else{removeModal()}
  })
  
  observeEvent(input$submit, handlerExpr = {
    withBusyIndicatorServer("submit",{
      data<-read.csv("https://www.dropbox.com/s/serwffb516rv843/selection_output.csv?dl=1", row.names = NULL)
      if(input$assessment>=0){
        if(is.na(data$FirstReview[paper_num()])){
          data$FirstReview[paper_num()] <- input$assessment
          data$FirstReviewer[paper_num()] <- tolower(trimws(input$name))
        } else{
          data$SecondReview[paper_num()] <- input$assessment
          data$SecondReviewer[paper_num()] <- tolower(trimws(input$name))
        }
        
        output$num <- renderText({
          num_complete = sum(data$FirstReviewer == tolower(trimws(input$name)),na.rm = TRUE) #Need to update to include second review
          if(is.na(num_complete)){
            num_complete = 0
          }
          paste(num_complete)
        })
        
        output$total <- renderText({
          num = sum(!is.na(data$FirstReviewer)) + sum(!is.na(data$SecondReviewer))
          total = nrow(data)*2
          paste0("Combined, we have screened ", num, " out of ", total, " abstracts")
        })
        
        num = sample(seq(1,nrow(data), by = 1)[is.na(data$FirstReview)],1)
        paper_num(num)
        updateSelectInput(session, "assessment", choices = c("Select answer"=-99,"Yes!"=1,"No"=0,"Unsure"=99))
        write.csv(data, "selection_output.csv", row.names = F)
        drop_upload("selection_output.csv")
      }
    })
  })
  
  output$name <- renderText({paste0("Hi ",input$name,"!")})
  
  output$num <- renderText({
    name_noSpace <- tolower(trimws(input$name))
    num = sum(data$FirstReviewer == name_noSpace,na.rm = T)
    if(is.na(num)){
      num = 0
    }
    paste(num)
  })
  
  output$total <- renderText({
    num = sum(!is.na(data$FirstReviewer)) + sum(!is.na(data$SecondReviewer))
    total = nrow(data)*2
    paste0("Combined, we have screened ", num, " out of ", total, " papers")
  })
  
  output$paper_name <- renderText({
    paste(data$TI[paper_num()])
    })
  output$abstract <- renderUI({
    if(nchar(data$AB[paper_num()])<1){
      div(tags$b("Abstract not found", style = "color: red;"))
    }else{paste(data$AB[paper_num()])}
    })
  output$link <- renderUI({
    if(nchar(data$DI[paper_num()])<1){
      div(tags$b("DOI not found", style = "color: red;"))
    }else{link <- paste0("https://doi.org/",data$DI[paper_num()])
    tags$a(href = link, link)}
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

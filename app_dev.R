library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(rdrop2)
drop_auth()

source("helpers.R") # Load all the code needed to show feedback on a button click

#data1 <- read_csv("savedrecs_001_500.csv")
#data2 <- read_csv("savedrecs_501_1000.csv")
#data3 <- read_csv("savedrecs_1001_1500.csv")
#data4 <- read_csv("savedrecs_1501_2000.csv")
#data5 <- read_csv("savedrecs_2001_2500.csv")
#data6 <- read_csv("savedrecs_2501_2956.csv")
#data_full <- data1%>% select(TI, AB, DI, AU, PY, DT)%>%
#  full_join(data2%>%  select(TI, AB, DI, AU, PY, DT))%>%
#  full_join(data3%>%  select(TI, AB, DI, AU, PY, DT))%>%
#  full_join(data4%>%  select(TI, AB, DI, AU, PY, DT))%>%
#  full_join(data5%>%  select(TI, AB, DI, AU, PY, DT))%>%
#  full_join(data6%>%  select(TI, AB, DI, AU, PY, DT))
#
#data_full2 <- data_full[data_full$DT %in% c("Article","Article; Book Chapter","Article; Data Paper","Article; Early Access","Article; Proceedings Paper","Proceedings Paper"),]
#data<-read.csv("https://www.dropbox.com/s/serwffb516rv843/selection_output.csv?dl=1", row.names = NULL)
#final <- data_full2%>%
#  left_join(data)
#write.csv(final, "selection_output2.csv", row.names = F)
#drop_upload("selection_output2.csv")

#
#write.csv(data_full,"selection_output.csv")
data<-read.csv("https://www.dropbox.com/s/xp0aels1d1eylgp/selection_output.csv?dl=1", row.names = NULL)
#data<-read.csv("selection_output.csv")
#data_col <- colnames(data)
#if(!"Ecological" %in% data_col){
#  data$Reviewer <- NA
#  data$NearTerm <- NA
#  data$Ecological <- NA
#  data$Forecast <- NA
#  data$Comment <- NA
#  write.csv(data, "selection_output.csv", row.names = F)
#  drop_upload("selection_output.csv")
#}

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
                    textOutput("total"),
                    conditionalPanel(
                      condition = "input.leader_boo == true",
                      hr(),
                      h3("Leaderboard"),
                      tableOutput("leaderboard")
                    ),
                    conditionalPanel(
                      condition = "input.meme_boo == true",
                      hr(),
                      imageOutput("meme")
                    )
                    )),
                
                # Output: 
                mainPanel(
                  h2("You have been assigned this paper:"),
                  strong(textOutput("paper_name")),
                  textOutput("authors"),
                  textOutput("year"),
                  h3("Abstract"),
                  htmlOutput("abstract"),
                  htmlOutput("link"),
                  h2("Screening questions:"),
                  strong("Is this a forecast/hindcast?"),
                  p("That is, is it designed to predict future conditions from the perspective of the model? Review papers and papers that are purely modeling should be marked NO"),
                  radioGroupButtons("forecast", 
                                    choiceNames = c("Yes","No","Unsure: requires a second review"), 
                                    choiceValues = c(1,0,99),
                                    selected = character(0)),
                  strong("Is it near-term (<10 year forecast horizon)?"),
                  radioGroupButtons("nearterm", 
                                    choiceNames = c("Yes","No","NA","Unsure: requires a second review"), 
                                    choiceValues = c(1,0,NA,99),
                                    selected = character(0)),
                  strong("Is it either biogeochemical or organismal (population or community)?"),
                  p("For example, discharge and temperature would NOT count, but oxygen, carbon dioxide, and nutrients would"),
                  radioGroupButtons("ecology", 
                                    choiceNames = c("Yes","No","Unsure: requires a second review"), 
                                    choiceValues = c(1,0,99),
                                    selected = character(0)),
                  textInput("comment",
                                    label = "Any other comments?"),
                  withBusyIndicatorUI(actionButton("submit","submit")),
                  br(),br(),br(),br(),br(),br(),br(),br()
                )
)

#Define server function
server <- function(input, output, session) {
  num = sample(seq(1,nrow(data), by = 1) )[is.na(data$Reviewer)],1)
  paper_num <- reactiveVal(value = num)
  results <- reactiveValues()
  
  nameModal <- function(real = TRUE) {
    modalDialog(
      span('What is your name?'),
      textInput(inputId = "name", label = " ", placeholder = "First Last"),
      if(!real){
        div(tags$b("Invalid name", style = "color: red;"))
      },
      checkboxInput("leader_boo", label = "Show leaderboard?"),
      checkboxInput("meme_boo", label = "Need inspiration?"),
      footer = tagList(
        actionButton("submit_name", "submit")
      )
    )
  }
  
  completeModal <- function() {
    modalDialog(
      easyClose = T,
      span('You did it!! Woohoo! 300 abstracts screened!')
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
      data<-read.csv("https://www.dropbox.com/s/xp0aels1d1eylgp/selection_output.csv?dl=1", row.names = NULL)
      data$Forecast[paper_num()] <- input$forecast
      data$NearTerm[paper_num()] <- input$nearterm
      data$Ecological[paper_num()] <- input$ecology
      data$Comment[paper_num()] <- input$comment
      data$Reviewer[paper_num()] <- tolower(trimws(input$name))
      write.csv(data, "selection_output.csv", row.names = F)
      drop_upload("selection_output.csv")
      
      output$num <- renderText({
        num_complete = sum(data$Reviewer == tolower(trimws(input$name)),na.rm = TRUE)
        if(num_complete == 300){
          showModal(completeModal())
        }
        paste(num_complete)
      })
      
      output$total <- renderText({
        num = sum(!is.na(data$Reviewer))
        total = nrow(data)
        paste0("Combined, we have screened ", num, " out of ", total, " abstracts")
      })
      
      num = sample(seq(1,nrow(data), by = 1)[is.na(data$Reviewer)],1)
      paper_num(num)
      updateRadioGroupButtons(session = session,
                              inputId = "forecast",
                              label = "Is this a forecast/hindcast?", 
                              choiceNames = c("Yes","No","Unsure: requires a second review"), 
                              choiceValues = c(1,0,99),
                              selected = character(0))
      updateRadioGroupButtons(session = session,
                              inputId = "nearterm",
                              label = "Is it near-term (<10 year forecast horizon)?", 
                              choiceNames = c("Yes","No","NA","Unsure: requires a second review"), 
                              choiceValues = c(1,0,NA,99),
                              selected = character(0))
      updateRadioGroupButtons(session = session,
                              inputId = "ecology",
                              label = "Is it a function of living things?", 
                              choiceNames = c("Yes","No","Unsure: requires a second review"), 
                              choiceValues = c(1,0,99),
                              selected = character(0))
      updateTextInput(session = session,
                      "comment",
                      label = "Any other comments?",
                      value = "")
      
      output$meme <- renderImage({
        files <- list.files("./memes")
        num = sum(data$Reviewer == tolower(trimws(input$name)),na.rm = TRUE)
        filename = paste0("./memes/",files[(num+1)%%length(files)+1])
        # Return a list containing the filename and alt text
        list(src = filename,
             alt = paste("[insert funny meme here]"),
             width = "100%")
      }, deleteFile = FALSE)
      
      output$leaderboard <- renderTable({
        leads <- summary(as.factor(data$Reviewer))
        leads <- as.data.frame(leads[-length(leads)])
        colnames(leads) <- c("Papers Screened")
        leads$Name <- rownames(leads)
        leads[order(leads$'Papers Screened', decreasing = T),c(2,1)]
      })
      
    })
    
  })
  
  output$name <- renderText({paste0("Hi ",input$name,"!")})
  
  output$num <- renderText({
    name_noSpace <- tolower(trimws(input$name))
    num = sum(data$Reviewer == name_noSpace,na.rm = T)
    if(is.na(num)){
      num = 0
    }
    paste(num)
  })
  
  output$total <- renderText({
    num = sum(!is.na(data$Reviewer))
    total = nrow(data)
    paste0("Combined, we have screened ", num, " out of ", total, " papers")
  })
  
  output$leaderboard <- renderTable({
    leads <- summary(as.factor(data$Reviewer))
    leads <- as.data.frame(leads[-length(leads)])
    colnames(leads) <- c("Papers Screened")
    leads$Name <- rownames(leads)
    leads[order(leads$'Papers Screened', decreasing = T),c(2,1)]
  })
  
  output$meme <- renderImage({
    files <- list.files("./memes")
    num = sum(data$Reviewer == tolower(trimws(input$name)),na.rm = TRUE)
    filename = paste0("./memes/",files[(num+1)%%length(files)+1])
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("[insert funny meme here]"),
         width = "100%")
  }, deleteFile = FALSE)
  
  output$paper_name <- renderText({
    paste(data$TI[paper_num()])
    })
  output$authors <- renderText({
    paste(data$AU[paper_num()])
  })
  output$year <- renderText({
    paste(data$PY[paper_num()])
  })
  output$abstract <- renderUI({
    if(is.na(data$AB[paper_num()])){
      div(tags$b("Abstract not found", style = "color: red;"))
    }else{paste(data$AB[paper_num()])}
    })
  output$link <- renderUI({
    if(is.na(data$DI[paper_num()])){
      div(tags$b("DOI not found", style = "color: red;"))
    }else{link <- paste0("https://doi.org/",data$DI[paper_num()])
    tags$a(href = link, link, target="_blank")}
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

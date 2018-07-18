library(shiny)
library(googlesheets)

ui <- function(req) {
  fluidPage(
    theme = "style.css",
    div(style = "display: none;",
        textInput("remote_addr", "remote_addr",
                  if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
                    req[["HTTP_X_FORWARDED_FOR"]]
                  else
                    req[["REMOTE_ADDR"]]
        )
    ),
    mainPanel(class = "grey",
      fluidRow(
        column(12, align = "center",
          h2(textOutput("question"))
        )
      ),
    
      br(),
    
      fluidRow(
        column(3, offset = 3, align = "center",
          uiOutput("answer1")
        ),
        column(3, align = "center",
          uiOutput("answer2")
        )
      ),
    
      br(),
    
      fluidRow(
        column(2, offset = 5, align = "center",
          uiOutput("cantdecide")
        )
      ),
    
      br()
      
      ),
    sidebarPanel(
      textInput("id", h3("Please enter your ID"), 
                value = "")
    )
  )
}

server <- function(input, output, session) {

  data <- gs_title("app")
  surveys <- gs_read(data, ws = "surveys", range = "A1:B100")
  question <- as.character(surveys[1,1])
  Filter(Negate(is.null), surveys)
  surveys[sapply(surveys, is.null)] <- NULL
  numAnswers <- dim(surveys)[1]
  currentLabel1 <- as.character(surveys[sample(1:numAnswers)[1], 2])
  currentLabel2 <- as.character(surveys[sample(1:numAnswers)[1], 2])
  
  #sets the label of the first button
  setLabel1 <- function(){
    currentLabel1 <<- as.character(surveys[sample(1:numAnswers)[1], 2])
    return(currentLabel1)
  }
  
  #sets the label of the second button
  setLabel2 <- function(){
    currentLabel2 <<- as.character(surveys[sample(1:numAnswers)[1], 2])
    if(currentLabel1 == currentLabel2) {
      setLabel2()
    }
    else {
      return(currentLabel2)
    }
  }
  
  #renders the question
  newQuestion <- function() {
    output$question <- renderText({
      question
    })
  }
  
  #renders the first answer button
  newButton1 <- function() {
    output$answer1 <- renderUI({
      actionButton("answer1", label = setLabel1(), class = "answer")
    })
  }
  
  #renders the second answer button
  newButton2 <- function() {
    output$answer2 <- renderUI({
      actionButton("answer2", label = setLabel2(), class = "answer")
    })
  }
  
  #renders the cantdecide button
  newButton3 <- function() {
    output$cantdecide <- renderUI({
      actionButton("cantdecide", label = "I  Can't Decide", class = "cantdecide")
    })
  }
  
  newQuestion()
  newButton1()
  newButton2()
  newButton3()
  
  #gets the label of the first button
  getLabel1 <- function(){
    return(currentLabel1)
  }
  
  #gets the label of the second button
  getLabel2 <- function(){
    return(currentLabel2)
  }
  
  #adds a row of data to the "responses" sheet
  addRow <- function(x){
    time <- format(Sys.time(), "%Y-%m-%d %H:%M %OS3")
    id <- input$id
    ip <- (isolate(input$remote_addr))
    if(x == 1) {
      gs_add_row(data, ws = "responses", input = data.frame(time, id, ip, question, getLabel1(), getLabel2(), getLabel1()))
    } else if(x == 2) {
      gs_add_row(data, ws = "responses", input = data.frame(time, id, ip, question, getLabel1(), getLabel2(), getLabel2()))
    } else {
      gs_add_row(data, ws = "responses", input = data.frame(time, id, ip, question, getLabel1(), getLabel2(), "Can't Decide"))
    }
  }
  
  observeEvent(input$answer1, {
    addRow(1)
    newButton1()
    newButton2()
  })
  
  observeEvent(input$answer2, {
    addRow(2)
    newButton1()
    newButton2()
  })
  
  observeEvent(input$cantdecide, {
    addRow(3)
    newButton1()
    newButton2()
    newButton3()
  })
  
}

# run the app
shinyApp(ui = ui, server = server)

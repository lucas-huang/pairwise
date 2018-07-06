library(shiny)
library(googlesheets)

ui <- function(req) {
  fluidPage(
    div(style = "display: none;",
        textInput("remote_addr", "remote_addr",
                  if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
                    req[["HTTP_X_FORWARDED_FOR"]]
                  else
                    req[["REMOTE_ADDR"]]
        )
    ),
    titlePanel(
      "Pairwise Comparison App"
    ),
    mainPanel(
      textOutput("question"),
      uiOutput("answer1"),
      uiOutput("answer2"),
      uiOutput("cantdecide")
    ),
    sidebarPanel(
      textInput("id", h3("Please enter your ID"), 
                value = "")
    )
  )
}

server <- function(input, output, session) {

  data <- gs_title("app")
  question <- as.character(gs_read(data, ws = "surveys", range = "A3:A3", col_names = FALSE))
  answers <- gs_read(data, ws = "surveys", range = "B2:B100")
  Filter(Negate(is.null), answers)
  answers[sapply(answers, is.null)] <- NULL
  numAnswers <- dim(answers)[1]
  currentLabel1 <- as.character(answers[sample(1:numAnswers)[1], 1])
  currentLabel2 <- as.character(answers[sample(1:numAnswers)[1], 1])
  
  #sets the label of the first button
  setLabel1 <- function(){
    currentLabel1 <<- as.character(answers[sample(1:numAnswers)[1], 1])
    return(currentLabel1)
  }
  
  #sets the label of the second button
  setLabel2 <- function(){
    currentLabel2 <<- as.character(answers[sample(1:numAnswers)[1], 1])
    if(currentLabel1 == currentLabel2) {
      setLabel2()
    }
    else {
      return(currentLabel2)
    }
  }
  
  output$question <- renderText({
    question
  })
  
  output$answer1 <- renderUI({
    actionButton("answer1", label = setLabel1())
  })
  
  output$answer2 <- renderUI({
    actionButton("answer2", label = setLabel2())
  })
  
  output$cantdecide <- renderUI({
    actionButton("cantdecide", label = "Can't Decide")
  })
  
  #gets the label of the first button
  getLabel1 <- function(){
    return(currentLabel1)
  }
  
  #gets the label of the second button
  getLabel2 <- function(){
    return(currentLabel2)
  }
  
  addRow <- function(x){
    time <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
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
  
  observeEvent(input$answer1,{
    addRow(1)
    setLabel1()
    setLabel2()
    updateActionButton(session, "answer1", label = currentLabel1)
    updateActionButton(session, "answer2", label = currentLabel2)
  })
  
  observeEvent(input$answer2, {
    addRow(2)
    updateActionButton(session, "answer1", label = setLabel1())
    updateActionButton(session, "answer2", label = setLabel2())
  })
  
  observeEvent(input$cantdecide, {
    addRow(3)
    updateActionButton(session, "answer1", label = setLabel1())
    updateActionButton(session, "answer2", label = setLabel2())
  })
  
}

# run the app
shinyApp(ui = ui, server = server)

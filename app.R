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
    )
  )
}

server <- function(input, output, session) {
  
  data <- gs_title("app")
  question <- as.character(gs_read(data, ws = "surveys", range = "A2:A2", col_names = FALSE))
  answers <- gs_read(data, ws = "surveys", range = "B2:B100")
  Filter(Negate(is.null), answers)
  answers[sapply(answers, is.null)] <- NULL
  numAnswers <- dim(answers)[1]
  randomAnswer1 <- answers[sample(1:numAnswers)[1], 1]
  randomAnswer2 <- answers[sample(1:numAnswers)[1], 1]
  currentLabel1 <- as.character(randomAnswer1)
  currentLabel2 <- as.character(randomAnswer2)
  ip <- isolate(input$remote_addr)
  
  output$question <- renderText({
    question
  })
  
  output$answer1 <- renderUI({
    actionButton("answer1", label = currentLabel1)
  })
  
  output$answer2 <- renderUI({
    actionButton("answer2", label = currentLabel2)
  })
  
  output$cantdecide <- renderUI({
    actionButton("cantdecide", label = "Can't Decide")
  })
  
  #sets the label of the first button
  setLabel1 <- function(){
    randomAnswer1 <<- answers[sample(1:numAnswers)[1], 1]
    currentLabel1 <<- as.character(randomAnswer1)
    return(currentLabel1)
  }
  
  #sets the label of the second button
  setLabel2 <- function(){
    randomAnswer2 <<- answers[sample(1:numAnswers)[1], 1]
    currentLabel2 <<- as.character(randomAnswer2)
    if(randomAnswer1 == randomAnswer2) {
      setLabel2()
    }
    else {
      return(currentLabel2)
    }
  }
  
  #gets the label of the first button
  getLabel1 <- function(){
    return(currentLabel1)
  }
  
  #gets the label of the second button
  getLabel2 <- function(){
    return(currentLabel2)
  }
  
  # store the counter outside your input/button
  vars = reactiveValues(counter = 0)
  
  # increase the counter
  observe({
    if(!is.null(input$click)){
      input$click
      isolate({
        vars$counter <- vars$counter + 1
      })
    }
  })
  
  observeEvent(input$answer1,{
    gs_add_row(data, ws = "responses", input = c(ip, question, getLabel1(), getLabel2(), getLabel1()))
    updateActionButton(session, "answer1", label = setLabel1())
    updateActionButton(session, "answer2", label = setLabel2())
  })
  
  observeEvent(input$answer2, {
    gs_add_row(data, ws = "responses", input = c(ip, question, getLabel1(), getLabel2(), getLabel2()))
    updateActionButton(session, "answer1", label = setLabel1())
    updateActionButton(session, "answer2", label = setLabel2())
  })
  
  observeEvent(input$cantdecide, {
    gs_add_row(data, ws = "responses", input = c(ip, question, getLabel1(), getLabel2(), "Can't Decide"))
    updateActionButton(session, "answer1", label = setLabel1())
    updateActionButton(session, "answer2", label = setLabel2())
  })
  
}

# run the app
shinyApp(ui = ui, server = server)

library(shiny)
library(tercen)
library(dplyr)
library(reshape2)
library(limma)
library(shinyjs)
library(shinydashboard)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  # Set appropriate options
  #options("tercen.serviceUri"="http://tercen:5400/api/v1/")
  #options("tercen.workflowId"= "4133245f38c1411c543ef25ea3020c41")
  #options("tercen.stepId"= "2b6d9fbf-25e4-4302-94eb-b9562a066aa5")
  #options("tercen.username"= "admin")
  #options("tercen.password"= "admin")
  ctx <- tercenCtx()
}
####
############################################

ui <- shinyUI(uiOutput("body"))

server <- shinyServer(function(input, output, session) {
  
  getData <- reactive({
    getInputData(session)
  })
  
  output$body = renderUI({
    dashboardPage(
      header = dashboardHeader(),
      sidebar = dashboardSidebar(shinyjs::useShinyjs(),
                                 tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
                                 tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
                                 sliderInput("thresh", "Threshold", min = 0, max = 1, value = 0.05),
                                 selectInput("sign", label = "Include if value is", choices = list("smaller than threshold", "greater than threshold"))),
      body = dashboardBody(fluidRow(box(title = "Venn diagram", width = 12, plotOutput("vd"))),
                           fluidRow(box(title = "Venn count", tableOutput("vc")),
                                    box(title = "Output data", HTML(paste("<center><h5>Click below to send data back to Tercen</h5>", 
                                                                          actionButton("button", "Transform data")),"</center>"))))
    )
  })
  
  
  observe({
    data <- getData()

    isolate({
      looksLikePValue = all(data >= 0) & all(data <= 1)
      if (!looksLikePValue) {
        updateSliderInput(session, "thresh", min = floor(min(data)), max =ceiling(max(data)), value = median(data))
      }
    })
    
    sign = reactive({
      if (input$sign == "greater than threshold"){
        sign = -1
      } else {
        sign = 1
      }
    })
    
    thr.data = reactive({
      if (any(is.na(data))){
        stop("Cannot make Venn diagram because the input data contains missing values.")
      }
      if (dim(data)[2] > 4){
        stop("For Venn plots there cannot be more than 4 columns in the cross-tab view")
      }
      
      sign() * data < sign() * input$thresh
    })
    
    output$vd = renderPlot({
      venn_data <- thr.data()
      limma::vennDiagram(venn_data, circle.col = (1:dim(venn_data)[2]) + 1)
    })
    
    output$vc = renderTable({
      counts <- limma::vennCounts(thr.data()) 
      result <- data.frame(counts[, 1:ncol(counts)])
      colnames(result) = colnames(counts)
      result
    })

    observeEvent(input$button, {
      shinyjs::disable("button")
      
      ctx <- getCtx(session)
      
      createOutput(thr.data()) %>% 
        ctx$addNamespace() %>%
        ctx$save()
    })
  })
})

getInputData <- function(session){
  ctx            <- getCtx(session)
  data           <- ctx$as.matrix()
  colnames(data) <- ctx$cselect() %>% pull()
  data
}

createOutput = function(df){
  result <- melt(df)
  nrows  <- nrow(result)/ncol(df)
  result %>% 
    select(value) %>%
    rename(included = value) %>%
    mutate(included = as.double(included)) %>%
    mutate(.ri = as.integer(c(seq(0,nrows-1), seq(0,nrows-1)))) %>% 
    mutate(.ci = as.integer(c(rep(0,nrows), rep(1,nrows)))) %>%
    select(.ri, .ci, included)
}

runApp(shinyApp(ui, server))  
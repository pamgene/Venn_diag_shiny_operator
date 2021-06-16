library(shiny)
library(tercen)
library(dplyr)
library(reshape2)
library(limma)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
  getData <- reactive({
    getInputData(session)
  })
  
  output$body = renderUI({
    mainPanel(
      h4("Venn Diagram"),
      sliderInput("thresh", "Threshold", min = 0, max = 1, value = 0.05),
      selectInput("sign", label = "Include if value is", choices = list("smaller than threshold", "greater than threshold") ),
      plotOutput("vd"),
      tags$hr(),
      tableOutput("vc")
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
  })
})

getInputData <- function(session){
  ctx <- getCtx(session)
  values <- list()
  
  data           <- ctx$as.matrix()
  colnames(data) <- ctx$cselect() %>% pull()
  data
}

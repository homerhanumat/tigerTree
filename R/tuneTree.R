#' Hand-Tune a Tree Model
#'
#' @param formula formula for \code{tree}.
#' @param data training data to make the tree models
#' @param testSet quiz data to try the models on
#' @param truth values of response variable in the quiz data
#'
#' @return No values returned
#' @export
tuneTree <- function(formula, data, testSet, truth) {

  # for tree control:
  nobs <- nrow(data)

  # determine tree type
  mod <- tree(formula, data)
  type <- summary(mod)$type
  classification <- grepl(pattern = "Classification", x = type)
  type <- ifelse(classification, "Class", "Reg")

  ## UI ---------------
  ui <- shinyUI(fluidPage(

    #  Application title
    title="Tune Your Tree",
    titlePanel("Tune Your Tree"),

    sidebarLayout(
      sidebarPanel(
        numericInput(inputId = "mincut", label = "mincut",
                     value = 5, step = 1, min = 1, max = floor(nobs/2))
        ,
        numericInput(inputId = "minsize", label = "minsize",
                     value = 10, step = 1, min = 2, max = nobs)
        ,
        numericInput(inputId = "mindev",label = "mindev",
                     value = 0.01, min = 0, max = 0.02)
        ,
        actionButton(inputId = "make", "Make Tree")
      )
      ,
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Tree Plot",
            plotOutput("treeplot")
          )
          ,
          tabPanel(
            title = "Summary/Try",
            verbatimTextOutput("summary"),
            verbatimTextOutput("try")
          )
          ,
          tabPanel(
            title = "Performance vs. Size",
            plotOutput("graph"),
            uiOutput("smooth"),
            h4("The most recent tree is in red")
          )
        )
      )
    )

  ))  #end fluidPage and and shinyUI

  ## Server ------------
  server <- function(input, output, session) {

    rv <- reactiveValues(
      xy = NULL,
      currentModel = NULL,
      currentPoint = NULL,
      try = NULL
    )

    # observeEvent(input$mincut,{
    #   updateNumericInput(session, inputId = "minsize", min = 2*input$mincut,
    #                      value = max(input$minsize, 2*input$mincut))
    # })
    #
    # observeEvent(input$minsize,{
    #   updateNumericInput(session, inputId = "mincut", min = 1,
    #                      value = min(floor(input$minsize/2), input$mincut),
    #                      max = floor(input$minsize/2))
    # })

    observeEvent(input$make, {
      req(input$make && 2*input$mincut <= input$minsize)
      mod <- tree(formula, data,
                  control = tree.control(
                    nobs = nobs,
                    mincut = input$mincut,
                    minsize = input$minsize,
                    mindev = input$mindev
                  ))
      rv$currentModel <- mod
      ourTry <- tryTree(mod = mod, testSet = testSet,
                        truth = truth, printOut = FALSE)
      rv$try <- ourTry
      nodes <- summary(mod)$size
      perf <- ifelse(type == "Class",
                     ourTry$error.rate,
                     ourTry$rmse)
      df <- data.frame(x = nodes, y = perf)
      rv$currentPoint <- data.frame(x = nodes, y = perf)
      newFrame <- rbind(rv$xy, df)
      rv$xy <- newFrame[!duplicated(newFrame),]
    })

    output$treeplot <- renderPlot({
      validate(
        need(2*input$mincut <= input$minsize,
             "minsize must be at least twice mincut")

      )
      req(rv$currentModel)
      model <- rv$currentModel
      plot(model); text(model)
    })

    output$summary <-renderPrint({
      validate(
        need(2*input$mincut <= input$minsize,
             "minsize must be at least twice mincut")

      )
      req(rv$currentModel)
      summary(rv$currentModel)
    })

    output$try <- renderPrint({
      req(rv$try)
      rv$try
    })

    output$smooth <- renderUI({
      req(rv$xy)
      if (nrow(rv$xy) >= 3) {
        checkboxInput("smooth", label ="Add Smoother")
      } else {
        return(NULL)
      }
    })

    perfGraph <- reactive({
      validate(
        need(2*input$mincut <= input$minsize,
             "minsize must be at least twice mincut")

      )
      req(rv$xy)
      df <- rv$xy
      if (type == "Class") {
        ylab <- "error rate on quiz set"
      } else {
        ylab <- "root mean square error on quiz set"
      }

      if (nrow(rv$xy) >= 1) {
        if (!is.null(input$smooth) && input$smooth) {
          p <- ggplot(data = df, mapping = aes(x = x, y = y)) +
            geom_point() + geom_smooth(se = FALSE) +
            geom_point(data = rv$currentPoint, size = 4, colour = "red")
          labs(x = "number of terminal nodes", y = ylab)
        } else {
          p <- ggplot(data = df, mapping = aes(x = x, y = y)) +
            geom_point() +
            geom_point(data = rv$currentPoint, size = 4, colour = "red")
          labs(x = "number of terminal nodes", y = ylab)
        }
        suppressWarnings(print(p))
      }
    })

    output$graph <- renderPlot({
        perfGraph()
    })

  }

  ## Make App ---------
  shinyApp(ui = ui, server = server)
}

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

  # useful for outputting code
  dfName <- deparse(substitute(data))

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
      sidebarPanel(width = 3,
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
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel(
            title = "Tree Plot",
            plotOutput("treeplot")
          )
          ,
          tabPanel(
            title = "Summary/Try",
            verbatimTextOutput("summary"),
            HTML("<br>"),
            htmlOutput("performance"),
            tableOutput("confusion")
            #verbatimTextOutput("try")
          )
          ,
          tabPanel(
            title = "Performance vs. Size",
            plotOutput("graph"),
            uiOutput("smooth"),
            h4("The most recent tree is in red")
          )
          ,
          tabPanel(
            title = "Models/Code",
            h4("Code for Selected Model(s)"),
            verbatimTextOutput("code"),
            h4("Models So Far (Select for Code)"),
            DT::dataTableOutput("models")
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
      try = NULL,
      record = NULL
    )

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

      if ( type == "Class" ) {
        newRecord <- data.frame(
          mincut = input$mincut,
          minsize = input$minsize,
          mindev = input$mindev,
          nodes = nodes,
          errRate = perf
        )
      } else {
        newRecord <- data.frame(
          mincut = input$mincut,
          minsize = input$minsize,
          mindev = input$mindev,
          nodes = nodes,
          rmse = perf
        )
      }
      rv$record <- rbind(rv$record, newRecord)
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

    output$performance <- renderUI({
      req(rv$try)
      if (type == "Class") {
        message <- paste0("<p>On quiz set, error rate is:  ", round(rv$try$error.rate,4),
                          ".  Confusion matrix is:<p>")
        HTML(message)
      } else {
        message <- paste0("<p>On quiz set, root mean square error is:  ", round(rv$try$rmse,3),
                          ".<p>")
        HTML(message)
      }
    })

    output$confusion <- renderTable({
      req(!is.null(rv$try) && type == "Class")
      rv$try$confusion
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

    output$models <- DT::renderDataTable({
      req(rv$record)
      rv$record
    }, server = TRUE)

    output$code <- renderPrint({
      req(input$models_rows_selected)
      rec <- input$models_rows_selected
      selected <- rv$record[rec,]
      code <- paste0("tr.model <- tree(", as.character(deparse(formula)),
                     ", data = ", dfName,
                     ",\n\t\tcontrol = tree.control(\n",
                     "\t\t\tnobs = nrow(", dfName,"), ",
                     "mincut = ",selected$mincut,",\n",
                     "\t\t\tminsize = ", selected$minsize, ", ",
                     "mindev = ", selected$mindev, "))\n")
      cat(code)
    })

  }

  ## Make App ---------
  shinyApp(ui = ui, server = server)
}

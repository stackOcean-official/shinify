# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Shiny Server
#'
#' This function creates a shiny server for your model
#' @param model Your R model
#' @param modeltype type of your model. Currently for "log_reg", "lin_reg","df_rpart", "dt_party" and "rf"
#' @keywords shiny
#' @export
#' @examples
#' shinify(model)
#' shinify(model, "log_reg")
#' shinify(model, "log_reg", "your awesome title")
shinify <- function(model, modeltype = "", title = "") {

  # load required packages
  if (!require(shiny)) {
    install.packages("shiny")
    library(shiny)
  }
  if (!require(shinythemes)) {
    install.packages("shinythemes")
    library(shinythemes)
  }
  if (modeltype == "dt_rpart" && !require(rpart)) {
    install.packages("rpart")
    library(rpart)
  }
  if (modeltype == "dt_party" && !require(party)) {
    install.packages("party")
    library(party)
  }
  if (modeltype == "svm" && !require(e1071)) {
    install.packages("e1071")
    library(e1071)
  }
  if (modeltype == "rf" && !require(randomForest)) {
    install.packages("randomForest")
    library(randomForest)
  }

  # set port for shiny server
  options(shiny.port = 8000)
  options(shiny.host = "0.0.0.0")

  # set attr names from model (first = output, rest = input)
  model_atributte <- paste(attr(model$terms, "predvars"))
  model_label <- model_atributte[-1]
  input_label <- model_label[-1]
  input_count <- length(input_label)

  # sigmoid function to correct output if using a log_reg
  sigmoid <- function(x) {
    result <- exp(x) / (1 + exp(x))
    return(result)
  }

  # Define UI
  ui <- fluidPage(
    theme = shinytheme("lumen"),
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        # multiple inputs depending on number of expeced regressors from the ml model
        numinputs <- lapply(1:input_count, function(i) {
          numericInput(inputId = paste0("num", i), label = input_label[i], value = 0)
        })
      ),

      # Output
      mainPanel(
        h2(model_label[1]),
        h2(textOutput(outputId = "prediction")),
        tags$a(href = "https://stackocean.com", "provided by stackOcean", target = "_blank")
      )
    )
  )

  # Define server function
  server <- function(input, output) {
    output$prediction <- renderText({
      df <- data.frame(matrix(ncol = input_count, nrow = 0))
      colnames(df) <- input_label
      df[1, ] <- sapply(1:input_count, function(i) {
        req(input[[paste0("num", i)]])
        input[[paste0("num", i)]]
      })
      # predict function
      predicted_output <- predict(model, newdata = df)

      # prepare output depending on the requirements by each ml model
      if (modeltype == "log_reg") {
        predicted_output <- sigmoid(predicted_output)
      }
      if (modeltype == "decision_tree") {
        predicted_output <- predicted_output[2]
      }
      paste(round(predicted_output, digits = 4))
    })
  }

  # Create Shiny object
  shinyApp(ui = ui, server = server)
}

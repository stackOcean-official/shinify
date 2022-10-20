# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Shiny Server
#'
#' This function creates a shiny server for your model
#' @param model Your R model
#' @param modeltype Abbreviation of your model type (e.g. "log_reg", "rf", ...). We are constantly working on adding new models and packages to support with shinify. Look up in jumpstart folder for currently supported models.
#' @param title Optional: add a Headline to your shiny server
#' @param attributes Change the displayed labels for your input and output variables. Mandatory if the passed model has no model terms.
#' @keywords shiny
#' @export
#' @examples
#' shinify(model)
#' shinify(model, "log_reg")
#' shinify(model, "log_reg", "your awesome title")
#' shinify(model, "log_reg", "your awesome title", c("output", "input1", "input2"))

shinify <- function(model, modeltype = "", title = "", attributes = c()) {

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
  if (!(is.null(model$terms)) && is.null(attributes)) {
    model_attributes <- paste(attr(model$terms, "predvars"))
    output_label <- model_attributes[2]
    input_label <- model_attributes[c(-1,-2)]
    input_count <- length(input_label)
  } else if (!(is.null(attributes)) && attributes > 1) {
    model_attributes <- attributes
    output_label <- model_attributes[1]
    input_label <- model_attributes[-1]
    input_count <- length(input_label)
  }
  if (is.null(attributes) && is.null(model$terms)) {
    stop("The passed model does not contain values for input & output labels and you have not passed your own. Considder adding a vector of attributs. Note: First value is output and the rest are the input values.")
  }

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
        h2(output_label),
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
      if (modeltype == "dt_rpart") {
        predicted_output <- predicted_output[2]
      }
      paste(round(predicted_output, digits = 4))
    })
  }

  # Create Shiny object
  shinyApp(ui = ui, server = server)
}

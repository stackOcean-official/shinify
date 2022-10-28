# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Shiny Server
#'
#' This function creates a shiny server for your model
#' @param model Your R model
#' @param modeltype Abbreviation of your model type (e.g. "log_reg", "rf"). We are constantly working on adding new models and packages to support with shinify. Look up in jumpstart folder for currently supported models.
#' @param title Optional: add a Headline to your shiny server
#' @param attr_names Change the displayed labels for your input and output variables. Mandatory if the passed model has no model terms. Note: the first value is output and the rest are the input values.
#' @param attr_types Change the type of your input and output variables (e.g. "numeric", "factor", "integer"). Mandatory if the passed model has no model terms. Note: the first value is output and the rest are the input values.
#' @keywords shiny
#' @export
#' @examples
#' shinify(model)
#' shinify(model, "log_reg")
#' shinify(model, "log_reg", "your awesome title")
#' shinify(model, "log_reg", "your awesome title", c("output", "input1", "input2"))
#' shinify(model, "log_reg", "your awesome title", c("output", "input1", "input2"), c("legendary", "attack", "defense", c("numeric", "numeric", "numeric")))


shinify <- function(model, modeltype = "", title = "", attr_names = c(), attr_types = c()) {
  # load required packages
  requiredPackages(modeltype)

  # set port for shiny server
  options(shiny.port = 8000)
  options(shiny.host = "0.0.0.0")

  # set attr names and type from model (first = output, rest = input)
  if (is.null(model$terms)) {
    stop_msg <- "function call:"
    if (is.null(attr_names)) {
      stop_msg <- paste(stop_msg, "\n You have not set the names for your model attributes and the passed model does not contain this information.\n Considder adding the vector `attr_names`.")
    }
    if (is.null(attr_types)) {
      stop_msg <- paste(stop_msg, "\n You have not set the type for your model attributes and the passed model does not contain this information.\n Considder adding the vector `attr_types`.")
    }
    stop_msg <- paste(stop_msg, "\n Note: First value is output and the rest are the input values.")
    stop(stop_msg)
  }

  if (!(is.null(model$terms)) && is.null(attr_names)) {
    model_attr_names <- paste(attr(model$terms, "predvars"))[-1]
    output_label <- model_attr_names[1]
    input_label <- model_attr_names[-1]
    input_count <- length(input_label)
  } else {
    model_attr_names <- attr_names
    output_label <- model_attr_names[1]
    input_label <- model_attr_names[-1]
    input_count <- length(input_label)
  }
  if (!(is.null(model$terms)) && is.null(attr_types)) {
    input_type <- paste(attr(model$terms, "dataClasses"))[-1]
  } else {
    input_type <- attr_types[-1]
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
        inputs <- lapply(1:input_count, function(i) {
          if (input_type[i] == "numeric" || input_type[i] == "integer" || input_type[i] == "double") {
            numericInput(inputId = paste0("num", i), label = input_label[i], value = 0)
          } else if (input_type[i] == "string" || input_type[i] == "factor") {
            textInput(inputId = paste0("num", i), label = input_label[i], value = "Text")
          }
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

# function to load all required packages
requiredPackages <- function(modeltype = "") {
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
}

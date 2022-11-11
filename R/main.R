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
#' @param attr_types Change the type of your input and output variables (e.g. "numeric", "factor", "integer"). Mandatory if the passed model has no model terms. Note: the first value is output and the rest are the input values. If CSV is part of your input types, it will ignore all others.
#' @keywords shiny
#' @export
#' @examples
#' shinify(model)
#' shinify(model, "log_reg")
#' shinify(model, "log_reg", "your awesome title")
#' shinify(model, "log_reg", "your awesome title", c("output", "input1", "input2"))
#' shinify(model, "log_reg", "your awesome title", c("output", "input1", "input2"), c("legendary", "attack", "defense"), c("numeric", "numeric", "numeric"))


shinify <- function(model, modeltype = "", title = "", attr_names = c(), attr_types = c()) {
  # load required packages
  requiredPackages(modeltype)

  # set port for shiny server
  options(shiny.port = 8000)
  options(shiny.host = "0.0.0.0")

  # check if given model has terms and attributes. If not and no additional information from the user stop the code and print msg.
  if (modeltype == "dt_party" || is.null(model$terms)) {
    stop_msg <- "function call:"
    count <- 0
    if (is.null(attr_names)) {
      stop_msg <- paste(stop_msg, "\n You have not set the names for your model attributes and the passed model does not contain this information.\n Considder adding the vector `attr_names`.")
      count <- count + 1
    }
    if (is.null(attr_types)) {
      stop_msg <- paste(stop_msg, "\n You have not set the type for your model attributes and the passed model does not contain this information.\n Considder adding the vector `attr_types`.")
      count <- count + 1
    }
    if (count >= 1) {
      stop_msg <- paste(stop_msg, "\n Note: First value is output and the rest are the input values.")
      stop(stop_msg)
    }
  }

  # set attr names and type from model (first = output, rest = input)
  if (is.null(attr_types)) {
    input_type <- paste(attr(model$terms, "dataClasses"))[-1]
  } else {
    input_type <- attr_types[-1]
  }

  if (is.element(tolower("csv"), tolower(input_type))) {
    csv_input <- TRUE
  } else {
    csv_input <- FALSE
  }

  if (is.null(attr_names)) {
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

  if (length(input_type) != input_count && !csv_input) {
    stop("Mismatch: The number of input variables is not determined correctly.\n If you set the attr_names or attr_types manually, make sure that they meet the requirements of the model.")
  }

  # Define UI
  ui <- fluidPage(
    theme = shinytheme("lumen"),
    titlePanel(title),
    sidebarLayout(
      if (csv_input) {
        sidebarPanel(
          textInput(inputId = "sep", label = "seperator", value = ";"),
          checkboxInput("header", "Header", TRUE),
          fileInput("upload", "Choose CSV File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          downloadButton("download")
        )
      } else {
        sidebarPanel(
          # multiple inputs depending on number of expeced regressors from the ml model
          inputs <- lapply(1:input_count, function(i) {
            if (input_type[i] == "numeric" || input_type[i] == "num" || input_type[i] == "integer" || input_type[i] == "double") {
              numericInput(inputId = paste0("num", i), label = input_label[i], value = 0)
            } else if (input_type[i] == "string" || input_type[i] == "factor") {
              textInput(inputId = paste0("num", i), label = input_label[i], value = "Text")
            }
          })
        )
      },

      # Output
      if (csv_input) {
        mainPanel(
          tags$a(href = "https://stackocean.com", "provided by stackOcean", target = "_blank"),
          h2("Table"),
          tableOutput("contents")
        )
      } else {
        mainPanel(
          h2(textOutput(outputId = "prediction")),
          tags$a(href = "https://stackocean.com", "provided by stackOcean", target = "_blank")
        )
      }
    )
  )

  # Define server function
  server <- function(input, output) {
    if (csv_input) {
      csv_data <- reactive({
        inFile <- input$upload
        if (is.null(inFile)) {
          return(NULL)
        }
        csv_data <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
        csv_data$output <- predict(model, newdata = csv_data)
        csv_data$output <- checkModeltypeRequirements(csv_data$output, modeltype)
        colnames(csv_data)[ncol(csv_data)] <- output_label
        csv_data
      })
    }
    # Only if input is CSV: Table of Input CSV with additional Column of prediced values
    output$contents <- renderTable({
      output <- csv_data()
    })
    # Only if input is CSV: Download button to download table of Input CSV with additional Column of prediced values
    output$download <- downloadHandler(
      filename = function() {
        paste0("results", ".csv")
      },
      content = function(file) {
        write.csv(csv_data(), file)
      }
    )
    # If input is NOT CSV: Predicts value for given input variables
    output$prediction <- renderText({
      df <- data.frame(matrix(ncol = input_count, nrow = 0))
      colnames(df) <- input_label
      df[1, ] <- sapply(1:input_count, function(i) {
        req(input[[paste0("num", i)]])
        input[[paste0("num", i)]]
      })
      # actual predict function
      predicted_output <- predict(model, newdata = df)
      predicted_output <- checkModeltypeRequirements(predicted_output, modeltype)
      paste(round(predicted_output, digits = 4))
    })
  }

  # Create Shiny object
  shinyApp(ui = ui, server = server)
}

checkModeltypeRequirements <- function(predicted_output, modeltype) {
  # prepare output depending on the requirements by each ml model
  if (modeltype == "log_reg") {
    predicted_output <- sigmoid(predicted_output)
  }
  if (modeltype == "dt_rpart") {
    predicted_output <- predicted_output[2]
  }
  return(predicted_output)
}

# sigmoid function to correct output if using a log_reg
sigmoid <- function(x) {
  result <- exp(x) / (1 + exp(x))
  return(result)
}

# function to load all required packages
requiredPackages <- function(modeltype) {
  if (!requireNamespace("shiny", quietly=TRUE)) {
    install.packages("shiny")
    library(shiny)
  }
  if (!requireNamespace("shinythemes", quietly=TRUE)) {
    install.packages("shinythemes")
    library(shinythemes)
  }
  if (modeltype == "dt_rpart" && !requireNamespace("rpart", quietly=TRUE)) {
    install.packages("rpart")
    library(rpart)
  }
  if (modeltype == "dt_party" && !requireNamespace("party", quietly=TRUE)) {
    install.packages("party")
    library(party)
  }
  if (modeltype == "svm" && !requireNamespace("e1071", quietly=TRUE)) {
    install.packages("e1071")
    library(e1071)
  }
  if (modeltype == "rf" && !requireNamespace("randomForest", quietly=TRUE)) {
    install.packages("randomForest")
    library(randomForest)
  }
}

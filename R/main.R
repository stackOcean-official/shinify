# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Shiny Server
#'
#' This function creates a shiny server for your model
#' @param model RModel, Your R model used in the prediction on your shiny server.
#' @param modeltype String, Abbreviation of your model type (e.g. "log_reg", "rf"). We are constantly working on adding new models and packages to support with shinify. Look up in jumpstart folder for currently supported models.
#' @param variables Vector, Set name of input variables your model is expecting. Optional if your model has 'model$terms' attribute. NOTE: if values are not equal to model$terms, you will get an error.
#' @param variable_types Vector, Set type of input variables your model is expecting. Optional if your model has 'model$terms' attribute. NOTE: if values are not equal to model$terms, you will get an error.
#' @param csv_upload Boolean, Set TRUE if you want to upload a CSV file as input. Default value is set to FALSE.
#' @param app_title String, Optional: Add a Headline to your shiny server
#' @param app_theme String, Optional: Set the shiny theme you want to use. Default theme is "lumen".
#' @param input_labels Vector, Optional: Set displayed name of your input variables. Does not effect the name of your input variables used in prediction.
#' @param output_label String, Optional: Set displayed name of your output variable. Does not effect the name of your output variable used in prediction.
#' @param default_input_values Vector, Optional: Set default values for your input variables when starting the shiny server.
#' @keywords shiny
#' @export
#' @examples
#' shinify(model)
#' shinify(model, "log_reg")
#' shinify(model, "log_reg", app_title = "your awesome title")
#' shinify(model, "log_reg", app_title = "your awesome title", csv_upload = TRUE)
#' shinify(model, "dt_party", app_title = "your awesome title", variables = c("attack", "defense"), variable_types = c("numeric", "numeric"))
#' shinify(model, "dt_party", app_title = "your awesome title", variables = c("attack", "defense"), variable_types = c("numeric", "numeric"), default_input_values = c("180", "290"))
#' @importFrom stats predict
#' @importFrom utils install.packages read.csv write.csv


shinify <- function(model, modeltype = "", variables = c(), variable_types = c(), csv_upload = FALSE, app_title = "Welcome to shinify", app_theme = "lumen", input_labels = c(), output_label = "", default_input_values = c(), runApp = TRUE) {
  library(shiny)
  library(openssl)

  # Check for directory and create if needed
  mainDir <- getwd()
  subDir <- "/shinify"
  modelDir <- "/shinify/model"
  if (!file.exists(subDir)) {
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    dir.create(file.path(mainDir, modelDir), showWarnings = FALSE)
  } else if (!file.exists(modelDir)) {
    dir.create(file.path(mainDir, modelDir), showWarnings = FALSE)
  }

  ################################################
  ## Check arguments and set internal variables ##
  ################################################

  if (nchar(modeltype) < 1) {
    warning("Warning: You have not passed the type of yor model. The passed modeltype effects your prediction and output values. The default modeltype is set to linear regression.")
  }

  # Stop if the number of model input names and type does not match up.
  if (!is.null(variables) && !is.null(variable_types)) {
    if (length(variables) != length(variable_types)) {
      stop("Missmatch: The number of set  names and types for model variables do not matchup.")
    }
  }

  if (xor(!is.null(variables), !is.null(variable_types))) {
    stop_msg <- "Error in shinify(): \n"
    if (!is.null(variables)) {
      stop_msg <- paste(stop_msg, "You have set the name for your variables, but not the type. Please add the 'varible_types' parameter.")
    } else {
      stop_msg <- paste(stop_msg, "You have set the type for your variables, but not the name Please add the 'varibles' parameter.")
    }
    stop(stop_msg)
  }

  # check for the type of independent variables. If not set by the user, we get them from the model.
  if (is.null(variable_types)) {
    input_type <- tryCatch(
      {
        paste(attr(model$terms, "dataClasses"))[-1]
      },
      error = function(e) {
        stop_msg <- "Error in shinify(): \n Your passed model does not contain the following information: model$terms."
        stop_msg <- paste(stop_msg, "\n Consider adding the vector `variable_types`.")
        if (is.null(variables)) {
          stop_msg <- paste(stop_msg, "\n Consider adding the vector `variables`.")
        }
        message(stop_msg)
        message("Here's the original error message:")
        message(e)
      }
    )
  } else {
    input_type <- sapply(variable_types, function(x) {
      if (tolower(x) == "numeric" || tolower(x) == "num" || tolower(x) == "integer" || tolower(x) == "int" || tolower(x) == "double") {
        return("numeric")
      } else if (tolower(x) == "string" || tolower(x) == "character") {
        return("character")
      } else if (tolower(x) == "factor") {
        return("factor")
      } else {
        stop("Error in shinify(): \n Your 'variable_types' do not meet the requirements of shinify(). \n You can choose between numeric, character and factor types.")
      }
    })
  }

  # check for the name of independent variables. If not set by the user, we get them from the model.
  if (is.null(variables)) {
    model_terms <- paste(attr(model$terms, "predvars"))[-1]
  } else {
    model_terms <- c("output", variables)
  }

  # the user can set the displayed name of his input and output variables. If not set by the user, they will be equal to the names of his model-terms
  if (is.null(input_labels)) {
    input_label <- model_terms[-1]
  } else {
    input_label <- input_labels
  }

  if (nchar(output_label) < 1) {
    output_label <- model_terms[1]
  } else {
    output_label <- output_label
  }

  input_count <- length(input_label)

  # the user can set default values for his input variables. If not set by the user, they will be 0 for numeric and "Text" for character and factor variables.
  if (is.null(default_input_values)) {
    input_values <- c()
    for (i in seq(input_count)) {
      if (input_type[i] == "numeric") {
        input_values[i] <- 0
      } else {
        input_values[i] <- "Text"
      }
    }
  } else {
    input_values <- default_input_values
  }

  ################################################
  ## Build UI                                   ##
  ################################################
  newScript <- paste0("
#
# This is a Shiny web application created with shinify(). You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about shinify() here:
#
#    https://github.com/stackOcean-official/shinify
#

", requiredPackages(modeltype), "

options(shiny.port = 8000)
options(shiny.host = '0.0.0.0')

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme('", app_theme, "'),
    titlePanel('", app_title, "'),
    sidebarLayout(")

  if (csv_upload) {
    newScript <- paste0(newScript, "
        sidebarPanel(
            textInput(inputId = 'sep', label = 'seperator', value = ';'),
            checkboxInput('header', 'Header', TRUE),
            fileInput('upload', 'Choose CSV File',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv'
                )
            ),
            downloadButton('download')
          ),
        mainPanel(
          tags$a(href = 'https://stackocean.com', 'provided by stackOcean', target = '_blank'),
          h2('Table'),
          tableOutput('contents')
          )")
  } else {
    newScript <- paste0(newScript, "
        sidebarPanel(")
    for (var in (1:input_count)) {
      if (input_type[var] == "numeric") {
        newScript <- paste0(newScript, "
            numericInput(inputId = paste0('num',", var, "), label = '", input_label[var], "', value = ", as.numeric(input_values[var]), "),")
      } else if (input_type[var] == "factor" || input_type[var] == "character") {
        newScript <- paste0(newScript, "
            textInput(inputId = paste0('num',", var, "), label = '", input_label[var], "', value = ", input_values[var], "),")
      }
    }
    newScript <- paste0(newScript, "
        ),
        mainPanel(
          h2('", output_label, "'),
          h2(textOutput(outputId = 'prediction')),
          tags$a(href = 'https://stackocean.com', 'provided by stackOcean', target = '_blank')
          )")
  }
  newScript <- paste0(newScript, "
      )
  )")

  ################################################
  ## Define Server Function                     ##
  ################################################
  if (csv_upload) {
    newScript <- paste0(newScript, "
  server <- function(input, output) {

      model <- readRDS('./model/model.rds')
      csv_data <- reactive({
        inFile <- input$upload
        if (is.null(inFile)) {
          return(NULL)
        }
        csv_data <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
        csv_data$output <- predict(model, newdata = csv_data)")
    if (modeltype == "dt_rpart") {
      newScript <- paste0(newScript, "
        csv_data$output <- csv_data$output[, 2]")
    }
    if (modeltype == "log_reg") {
      newScript <- paste0(newScript, "
        csv_data$output <- sigmoid(csv_data$output)")
    }
    newScript <- paste0(newScript, "
        colnames(csv_data)[ncol(csv_data)] <- '", output_label, "'
        csv_data
      })
      output$contents <- renderTable({
        output <- csv_data()
      })
      output$download <- downloadHandler(
        filename = function() {
          paste0('results', '.csv')
      },
      content = function(file) {
        write.csv(csv_data(), file)
      }
    )
  }")
  } else {
    model_terms_string <- paste0("'", model_terms[-1], "'", collapse = ",")
    model_terms_string <- paste0("c(", model_terms_string, ")")
    newScript <- paste0(newScript, "
server <- function(input, output, session) {

    model <- readRDS('./model/model.rds')
    output$prediction <- renderText({
        df <- data.frame(matrix(ncol =", input_count, ", nrow = 0))
        colnames(df) <-", model_terms_string, "
        df[1, ] <- sapply(1:", input_count, ", function(i) {
          req(input[[paste0('num', i)]])
          input[[paste0('num', i)]]
        })
    # actual predict function and additional function call for sigmoid if needed
    predicted_output <- tryCatch(
        {
          predict(model, newdata = df)
        },
        error = function(e) {
          message('Error in shinify(): Your passed values do not match with your model. NOTE: the column names of your training data have to match the variables names.')
          message('Here is the original warning message:')
          message(e)
        }
    )")
    if (modeltype == "log_reg") {
      newScript <- paste0(newScript, "
      predicted_output <- sigmoid(predicted_output)")
    }
    if (modeltype == "dt_rpart") {
      newScript <- paste0(newScript, "
      predicted_output <- predicted_output[2]")
    }
    newScript <- paste0(newScript, "
      paste(round(predicted_output, digits = 4))
    })
    session$onSessionEnded(function() {
      cat('closing shiny app...')
      stopApp()
  })
}")
  }
  if (modeltype == "log_reg") {
    newScript <- paste0(newScript, "
  # sigmoid function to correct output if using a log_reg
  sigmoid <- function(x) {
    result <- exp(x) / (1 + exp(x))
    return(result)
  }")
  }

  newScript <- paste0(newScript, "
  shinyApp(ui = ui, server = server)")

  ################################################
  ## Save Shiny App and Model                   ##
  ################################################

  fileConn <- file("shinify/.app.R")
  writeLines(newScript, fileConn)
  close(fileConn)

  if (file.exists("shinify/app.R")) {
    compareScripts <- tools::md5sum(c("shinify/.app.R", "shinify/app.R"))
    if (compareScripts[1] != compareScripts[2]) {
      cat("An app.R file already exists and will be overwritten.")
      check <- menu(c("Yes", "No"), title = "Do you wish to continue? (y/n)")
      print(check)
      if (check == 2) stop("Process aborted.", call. = FALSE)
      cat(paste0("Overwriting app.R in ", mainDir, subDir), fill = TRUE)
      fileConn <- file("shinify/app.R")
      writeLines(newScript, fileConn)
      close(fileConn)
    }
  } else {
    cat(paste0("Writing app.R to", mainDir, subDir), fill = TRUE)
    fileConn <- file("shinify/app.R")
    writeLines(newScript, fileConn)
    close(fileConn)
  }

  if (file.exists("shinify/model/model.rds")) {
    savedModel <- readRDS("shinify/model/model.rds")
    msg <- tryCatch(
      {
        compareModel <- md5(c(toString(savedModel), toString(model)))
        if (compareModel[1] != compareModel[2]) {
          msg <- "A different model.RDS file already exists and will be overwritten. "
        } else {
          msg <- ""
        }
        msg
      },
      error = function(e) {
        return("A model.RDS file already exists that can not be compared and will be overwritten. ")
      }
    )
    if (nchar(msg) > 1) {
      cat(msg)
      checkModel <- menu(c("Yes", "No"), title = "Do you wish to continue? (y/n)")
      if (checkModel == 2) stop("Process aborted.", call. = FALSE)
      cat(paste0("Overwriting model.RDS in ", mainDir, modelDir), fill = TRUE)
      saveRDS(model, "shinify/model/model.rds")
    }
  } else {
    cat(paste0("Writing model.RDS to", mainDir, modelDir), fill = TRUE)
    saveRDS(model, "shinify/model/model.rds")
  }

  file.remove("shinify/.app.R")

  if (runApp) {
    cat("starting shiny app...", fill = TRUE)
    runApp("./shinify")
  }
}


# function to load all required packages TODO: check if needed
requiredPackages <- function(modeltype) {
  reqPackages <- "
library(shiny)
library(shinythemes)
  "
  if (!requireNamespace("shiny", quietly = TRUE)) {
    install.packages("shiny", repos = "http://cran.us.r-project.org")
  }
  if (!requireNamespace("shinythemes", quietly = TRUE)) {
    install.packages("shinythemes", repos = "http://cran.us.r-project.org")
  }
  if (modeltype == "dt_rpart" && !requireNamespace("rpart", quietly = TRUE)) {
    install.packages("rpart", repos = "http://cran.us.r-project.org")
    reqPackages <- paste0(reqPackages, "
library(rpart)")
  }
  if (modeltype == "dt_party" && !requireNamespace("party", quietly = TRUE)) {
    install.packages("party", repos = "http://cran.us.r-project.org")
    reqPackages <- paste0(reqPackages, "
library(party)")
  }
  if (modeltype == "svm" && !requireNamespace("e1071", quietly = TRUE)) {
    install.packages("e1071", repos = "http://cran.us.r-project.org")
    reqPackages <- paste0(reqPackages, "
library(e1071)")
  }
  if (modeltype == "rf" && !requireNamespace("randomForest", quietly = TRUE)) {
    install.packages("randomForest", repos = "http://cran.us.r-project.org")
    reqPackages <- paste0(reqPackages, "
library(randomForest)")
  }
  return(reqPackages)
}

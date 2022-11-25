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


shinify <- function(model, modeltype = "", variables = c(), variable_types = c(), csv_upload = FALSE, app_title = "Welcome to shinify", app_theme = "lumen", input_labels = c(), output_label = "", default_input_values = c()) {
  # load required packages depending on the modeltype
  requiredPackages(modeltype)

  # set port for shiny server
  options(shiny.port = 8000)
  options(shiny.host = "0.0.0.0")

  ################################################
  ## Check arguments and set internal variables ##
  ################################################

  if (nchar(modeltype) < 1) {
    warning("You have not passed the type of yor model. The passed modeltype effects your prediction and output values. The default modeltype is set to linear regression.")
  }

  # Stop if the number of model input names and type does not match up.
  if (!is.null(variables) && !is.null(variable_types)) {
    if (length(variables) != length(variable_types)) {
      stop("Missmatch: The number of set  names and types for model variables does not matchup.")
    }
  }

  if (xor(!is.null(variables), !is.null(variable_types))) {
    if (!is.null(variables)) {
      stop_msg <- "You have set the name for your variables, but not the type. Please add the 'varible_types' parameter."
    } else {
      stop_msg <- "You have set the type for your variables, but not the name Please add the 'varibles' parameter."
    }
    stop(stop_msg)
  }

  # check if given model has terms and attributes. So for only dt_party has no model-terms. If not and no additional information from the user stop the code and print msg.
  if (modeltype == "dt_party" || is.null(model$terms)) {
    stop_msg <- "function call:"
    count <- 0
    if (is.null(variables)) {
      stop_msg <- paste(stop_msg, "\n You have not set the names for your model attributes and the passed model does not contain this information.\n Considder adding the vector `attr_names`.")
      count <- count + 1
    }
    if (is.null(variable_types)) {
      stop_msg <- paste(stop_msg, "\n You have not set the type for your model attributes and the passed model does not contain this information.\n Considder adding the vector `attr_types`.")
      count <- count + 1
    }
    if (count >= 1) {
      stop_msg <- paste(stop_msg, "\n Note: First value is output and the rest are the input values.")
      stop(stop_msg)
    }
  }

  # check for the type of dependent variables. If not set by the user, we get them from the model.
  if (is.null(variable_types)) {
    input_type <- paste(attr(model$terms, "dataClasses"))[-1]
  } else {
    input_type <- sapply(variable_types, function(x) {
      if (tolower(x) == "numeric" || tolower(x) == "num" || tolower(x) == "integer" || tolower(x) == "int" || tolower(x) == "double") {
        return("numeric")
      } else if (tolower(x) == "string" || tolower(x) == "character") {
        return("character")
      } else if (tolower(x) == "factor") {
        return("factor")
      } else {
        return(NULL)
      }
    })
  }

  # check for the name of dependent variables. If not set by the user, we get them from the model.
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

  # the user can set default values for his input variables. If not set by the user, they will be 0 for numeric und "Text" for character and factor varibles.
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
  ## Define UI                                  ##
  ################################################
  ui <- fluidPage(
    theme = shinytheme(app_theme),
    titlePanel(app_title),
    # Build the sidebar / input section depending on the input type. For CSV we create a file-upload and a download button. For single varibales we create input fields.
    sidebarLayout(
      if (csv_upload) {
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
          # multiple inputs depending on number of expected inputs from the model and the type of each input
          inputs <- lapply(1:input_count, function(i) {
            if (input_type[i] == "numeric") {
              numericInput(inputId = paste0("num", i), label = input_label[i], value = as.numeric(input_values[i]))
            } else if (input_type[i] == "factor" || input_type[i] == "character") {
              textInput(inputId = paste0("num", i), label = input_label[i], value = input_values[i])
            }
          })
        )
      },

      # Build the main panel / output section depending on the input type. For CSV we create a table of the input csv and a row of outputs. For single variables we create a textfield that displays the result.
      if (csv_upload) {
        mainPanel(
          tags$a(href = "https://stackocean.com", "provided by stackOcean", target = "_blank"),
          h2("Table"),
          tableOutput("contents")
        )
      } else {
        mainPanel(
          h2(output_label),
          h2(textOutput(outputId = "prediction")),
          tags$a(href = "https://stackocean.com", "provided by stackOcean", target = "_blank")
        )
      }
    )
  )

  ################################################
  ## Define Server Function                     ##
  ################################################
  server <- function(input, output, session) {
    # Load server functions depending on the input. For CSV we start a reactive Function to read an uploaded csv and addend a column with predicted output
    if (csv_upload) {
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
    # Only if input is CSV: Render table of input CSV with additional column of predicted values
    output$contents <- renderTable({
      output <- csv_data()
    })
    # Only if input is CSV: Download button to download table of Input CSV with additional Column of predicted values
    output$download <- downloadHandler(
      filename = function() {
        paste0("results", ".csv")
      },
      content = function(file) {
        write.csv(csv_data(), file)
      }
    )
    # If input is NOT CSV: Predicts value for given input variables. Therefore we create a data frame of one row containing the given input values.
    output$prediction <- renderText({
      df <- data.frame(matrix(ncol = input_count, nrow = 0))
      colnames(df) <- model_terms[-1]
      df[1, ] <- sapply(1:input_count, function(i) {
        req(input[[paste0("num", i)]])
        input[[paste0("num", i)]]
      })
      # actual predict function and additional function call for sigmoid if needed
      predicted_output <- predict(model, newdata = df)
      predicted_output <- checkModeltypeRequirements(predicted_output, modeltype)
      paste(round(predicted_output, digits = 4))
    })
  }

  ################################################
  ## Create Shiny Object                        ##
  ################################################
  shinyApp(ui = ui, server = server)
}

################################################
## Additional Function Calls                  ##
################################################
# prepare output depending on the requirements by each ml model
checkModeltypeRequirements <- function(predicted_output, modeltype) {
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
  if (!requireNamespace("shiny", quietly = TRUE)) {
    install.packages("shiny", repos = "http://cran.us.r-project.org")
  }
  if (!requireNamespace("shinythemes", quietly = TRUE)) {
    install.packages("shinythemes", repos = "http://cran.us.r-project.org")
  }
  if (modeltype == "dt_rpart" && !requireNamespace("rpart", quietly = TRUE)) {
    install.packages("rpart", repos = "http://cran.us.r-project.org")
    library(rpart)
  }
  if (modeltype == "dt_party" && !requireNamespace("party", quietly = TRUE)) {
    install.packages("party", repos = "http://cran.us.r-project.org")
    library(party)
  }
  if (modeltype == "svm" && !requireNamespace("e1071", quietly = TRUE)) {
    install.packages("e1071", repos = "http://cran.us.r-project.org")
    library(e1071)
  }
  if (modeltype == "rf" && !requireNamespace("randomForest", quietly = TRUE)) {
    install.packages("randomForest", repos = "http://cran.us.r-project.org")
    library(randomForest)
  }
  library(shiny)
  library(shinythemes)
}

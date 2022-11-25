test_that("test dt_party", {
  install.packages("party", repos = "http://cran.us.r-project.org")
  library(party)
  data <- read.csv("https://github.com/stackOcean-official/hostr/files/9681827/pokemon.csv")

  # create variables
  legendary <- data$is_legendary
  attack <- data$attack
  defense <- data$defense

  # split train and test data
  data <- data.frame(legendary, attack, defense)
  data_train <- data[1:(nrow(data) - 100), ]
  # actual linear regression
  dt <- ctree(legendary ~ attack + defense, data = data_train)
  expect_no_error(
    shinify(dt, modeltype = "dt_party", variables = c("attack", "defense"), variable_types = c("numeric", "numeric"), app_title = "Hello", output_label = "3")
  )
  expect_no_error(
    shinify(dt, modeltype = "dt_party", variables = c("attack", "defense"), variable_types = c("numeric", "numeric"), app_theme = "lumen")
  )
  expect_no_error(
    shinify(dt, modeltype = "dt_party", variables = c("attack", "defense"), variable_types = c("numeric", "numeric"), csv_upload = TRUE)
  )
  expect_no_error(
    shinify(dt, modeltype = "dt_party", variables = c("attack", "defense"), variable_types = c("numeric", "numeric"))
  )
  expect_error(
    shinify(dt, modeltype = "dt_party")
  )
  expect_error(
    shinify(dt, modeltype = "dt_party", variables = c("attack", "defense"))
  )
  expect_error(
    shinify(dt, modeltype = "dt_party", variable_types = c("numeric", "numeric"))
  )
})

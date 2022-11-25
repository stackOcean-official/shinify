test_that("Test linear regression", {
  data <- read.csv("https://github.com/stackOcean-official/hostr/files/9681827/pokemon.csv")

  # create variables
  legendary <- data$is_legendary
  attack <- data$attack
  defense <- data$defense

  # split train and test data
  data <- data.frame(legendary, attack, defense)
  data_train <- data[1:(nrow(data) - 100), ]
  # actual linear regression
  lin_reg <- lm(legendary ~ attack + defense, data = data_train)
  expect_warning(shinify(lin_reg))
  expect_error(shinify(model))
  expect_no_error(shinify(lin_reg, modeltype = "lin_reg"))
  expect_no_error(shinify(lin_reg, modeltype = "lin_reg", input_labels = c("1", "2")))
  expect_no_error(shinify(lin_reg, modeltype = "lin_reg", app_title = "Hello"))
  expect_no_error(shinify(lin_reg, modeltype = "lin_reg", app_title = "Hello", app_theme = "superhero"))
  expect_no_error(shinify(lin_reg, modeltype = "lin_reg", app_title = "Hello", app_theme = "superhero", csv_upload = TRUE))
})

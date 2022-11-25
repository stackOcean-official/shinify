test_that("test dt_party success", {
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
    shinify(dt, modeltype = "dt_party", variables = c("attack", "defense"), variable_types = c("numeric", "numeric"))

  )
  expect_error(
    shinify(dt, modeltype = "dt_party")
  )
})

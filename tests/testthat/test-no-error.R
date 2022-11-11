test_that("Test no Error", {
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
  expect_no_error(shinify(lin_reg))
})

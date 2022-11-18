test_that("Test no Error", {
  data <- read.csv("https://github.com/stackOcean-official/hostr/files/9681827/pokemon.csv")

  # create variables
  legendary <- data$is_legendary
  attack <- data$attack
  defense <- data$defense

  # split train and test data
  data <- data.frame(legendary, attack, defense)
  data_train <- data[1:(nrow(data) - 100), ]
  data_test <- data[(nrow(data) - 99):nrow(data), ]

  # actual linear regression
  lin_reg <- lm(legendary ~ attack + defense, data = data_train)
  df <- data.frame(data_test[, c(2, 3)], predict(lin_reg, data_test))

  expect_no_error(shinify(lin_reg))

  testServer(
    # expected problem: in server function the boolean csv_input is not set -> CSV upload not enabled -> can't access csv_data() function
    shinify(lin_reg, modeltype = "lin_reg", title = "your title here", attr_types = c("num", "csv")),
    args = list(data_test, df),{
      session$setInputs(upload = data_test[, c(2, 3)])
      expect_equal(csv_data(), df)
    })

})

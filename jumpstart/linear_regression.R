library(shinify)

# load data
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
summary(lin_reg)

# input for new prediction
attack <- 120
defense <- 290
test_data_new <- data.frame(attack, defense)

# actual prediction that pokemon is legendary
predict(lin_reg, test_data_new)

# shinify linear regression
shinify(lin_reg, modeltype = "lin_reg", title = "your title here")

a <- NULL
b <- NULL
c <- NULL
if (is.null(a)) {
  stop_msg <- ""
  if (is.null(b)) {
    stop_msg <- paste(stop_msg, "You have not set the names for your model attributes and the passed model does not contain this information. Considder adding the vector `attr_names` to your function call. Note: First value is output and the rest are the input values.")
  } else if (is.null(c)) {
    stop_msg <- paste(stop_msg, "You have not set the type for your model attributes and the passed model does not contain this information. Considder adding the vector `attr_types` to your function call. Note: First value is output and the rest are the input values.")
  }
  print(stop_msg)
}

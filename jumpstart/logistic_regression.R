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

# actual logistic regression
log_reg <- glm(legendary ~ attack + defense, data = data_train, family = binomial())
summary(log_reg)

# input for new prediction
attack <- 120
defense <- 290
test_data_new <- data.frame(attack, defense)

# definition of a sigmoid function to normalize predictions
sigmoid <- function(x) {
  result <- exp(x) / (1 + exp(x))
  return(result)
}

# actual predicted percentage that pokemon is legendary with glm model
sigmoid(predict(log_reg, test_data_new))

# shinify logistic model
shinify(log_reg, modeltype = "log_reg")

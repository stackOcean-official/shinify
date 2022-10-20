library(shinify)
library(randomForest)

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

# actual random forest
rf_mod <- randomForest(legendary ~ attack + defense, data = data_train)
summary(rf_mod)

# input for new prediction
attack <- 120
defense <- 290
test_data_new <- data.frame(attack, defense)

# actual prediction that pokemon is legendary
predict(rf_mod, newdata = data_test)

# shinify random forest model
shinify(rf_mod, modeltype = "rf", title = "your title here")

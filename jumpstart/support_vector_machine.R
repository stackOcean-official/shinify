library(shinify)
library(e1071)

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

# actual svm
svm_mod <- svm(legendary ~ attack + defense, data = data_train)
summary(svm_mod)

# input for new prediction
attack <- 120
defense <- 290
test_data_new <- data.frame(attack, defense)

# actual predicted percentage that pokemon is legendary with decision tree
predict(svm_mod, test_data_new)

# shinify logistic model
shinify(svm_mod, modeltype = "svm", title = "your title here")

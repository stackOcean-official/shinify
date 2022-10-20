library(shinify)
library(naivebayes)

# load data
data <- read.csv("https://github.com/stackOcean-official/hostr/files/9681827/pokemon.csv")

# create variables
legendary <- data$is_legendary
attack <- data$attack
defense <- data$defense

# split train and test data
data <- data.frame(legendary, attack, defense)
data$legendary = as.factor(data$legendary)
data_train <- data[1:(nrow(data) - 100), ]
data_test <- data[(nrow(data) - 99):nrow(data), ]

# actual svm
nb_mod <- naive_bayes(legendary ~ attack + defense, data = data_train)
nb_mod
summary(nb_mod)

# input for new prediction
attack <- 120
defense <- 290
test_data_new <- data.frame(attack, defense)

# actual predicted percentage that pokemon is legendary with decision tree
predict(nb_mod, test_data_new, "prob")

# shinify logistic model
shinify(nb_mod, modeltype = "nb", title = "your title here", atributtes = c("output", "input 1", "input 2", "unused input 3"))

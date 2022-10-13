library(shinify)
library(party)

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

# actual decision tree
dt <- ctree(legendary ~ attack + defense, data = data_train)
summary(dt)

# actual prediction that pokemon is legendary
predict(dt, data_test, type = "response")

# shinify decision tree
shinify(dt, modeltype = "dt_party", title = "your title here")

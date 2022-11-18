# local testscript
library(shinify)
library(party)
library(rpart)
library(knn)
library(randomForest)
library(e1071)


data <- read.csv("https://github.com/stackOcean-official/hostr/files/9681827/pokemon.csv")

# create variables
legendary <- data$is_legendary
attack <- data$attack
defense <- data$defense

# split train and test data
data <- data.frame(legendary, attack, defense)
data_train <- data[1:(nrow(data) - 100), ]

lin_reg <- lm(legendary ~ attack + defense, data = data_train)
dt <- ctree(legendary ~ attack + defense, data = data_train)
dt <- rpart(legendary ~ attack + defense, data = data_train, method = "class")
knn_mod <- knn(legendary ~ attack + defense, data = data_train, k = 5)
log_reg <- glm(legendary ~ attack + defense, data = data_train, family = binomial())
rf_mod <- randomForest(legendary ~ attack + defense, data = data_train)
svm_mod <- svm(legendary ~ attack + defense, data = data_train)

shinify(lin_reg, modeltype = "lin_reg", title = "your title here")
shinify(dt, modeltype = "dt_party", title = "your title here", attr_names = c("legendary", "attack", "defense"), attr_types = c("num", "num", "num"))
shinify(dt, modeltype = "dt_rpart", title = "your title here")
shinify(knn_mod, modeltype = "knn", title = "your title here")
shinify(log_reg, modeltype = "log_reg", title = "your title here")
shinify(rf_mod, modeltype = "rf", title = "your title here")
shinify(svm_mod, modeltype = "svm", title = "your title here")


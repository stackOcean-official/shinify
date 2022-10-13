library(shinify)
library(rpart)

# load data
data = read.csv("https://github.com/stackOcean-official/hostr/files/9681827/pokemon.csv")

# create variables
legendary = data$is_legendary
attack = data$attack
defense = data$defense

# split train and test data
data = data.frame(legendary, attack, defense)
data_train = data[1:(nrow(data) - 100), ]
data_test = data[(nrow(data) - 99):nrow(data), ]

# actual decision tree
d_tree = rpart(legendary ~ attack + defense, data = data_train, method="class")

# input for new prediction
attack = 120
defense = 290
test_data_new = data.frame(attack, defense)

# actual prediction that the pokemon is legendary
predict(d_tree, newdata = test_data_new)

# shinify decision tree
shinify(d_tree, modeltype = "d_tree", title = "your title here")
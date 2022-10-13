library(shinify)
library(party)

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
d_tree = ctree(legendary ~ attack + defense, data = data_train)

# actual prediction that pokemon is legendary 
predict(d_tree, data_test, type="response")

# shinify decision tree
shinify(d_tree, modeltype = "d_tree", title = "your title here")

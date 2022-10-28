# shinify

## No more coding needed, just add one line to your script in which you call our magic shinify function.

Shinify automatically creates a shiny server and visual interface for you to interact with your machine learning or statistical model.

> :warning: This repository is still in an early stage of development and the functions are limited. We are constantly working on adding new models and packages to support with shinify. Take a look in the jumpstart folder for currently supported models and libraries. We love the open source community and want to show what we are working on early. We will update this readme with more information. Until then, feel free to share your thoughts, contact us, and contribute if you'd like.

---

## How to use

Install the package from GitHub (If you don't have devtools, install it first: `install.packages("devtools")`):

```
devtools::install_github("stackOcean-official/shinify")
```

In your code load the shinify package and after that, just hand your model over to our `shinify` function to start a R Shiny server and let shinify figure out all the configuration.

```r
# load shinify package
library(shinify)

# do your logistic regression or other algorithms
log_reg <- glm(...)

# let shinify transform it into a shiny server
shinify(log_reg)
```

![shinify-example](https://user-images.githubusercontent.com/675065/196923840-11cb971b-990f-46b2-a389-de92e3d1fa44.png)

Here is the full example for a logistic regression. You can find more examples in the [jumpstart folder](https://github.com/stackOcean-official/shinify/tree/main/jumpstart)
:

```r
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
shinify(log_reg, modeltype = "log_reg", title = "your title here")

```

Note that you can only host one model at a time in the current development status.

---

After calling the `shinify()` method with the model, a shiny server is started where you can interact with your own model via a graphical interface.
![shiny-server-preview](https://user-images.githubusercontent.com/28595283/194275509-2faa8937-922a-4006-978e-9f82b0044e04.png)

## Shinify function in detail

The `shinify()` function creates a shiny server for your model

| Prop       | Type   | Required | Default | Description                                                                                                                                        |
| ---------- | ------ | -------- | ------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| model      | model  | `yes`    | ""      | Your R model (output from statistics / machine learning algorithm)                                                                                 |
| modeltype  | string | `no`     | ""      | Abbreviation of your model type that determines. See table below for possible configuration                                                        |
| title      | string | `no`     | ""      | Sets the title visible in the shiny application                                                                                                    |
| attr_names | vector | `no`     | c()     | Change the displayed labels for your input and output variables (first element is output label). Mandatory if the passed model has no model terms.
| attr_types | vector | `no`      | c()    | Change the type for your input and output variables (first element is output type). Mandatory if the passed model has no model terms. |                                        
These are the currently available options for `modeltype`. We are constantly working on adding new models and packages to support with shinify. Please [write an issue](https://github.com/stackOcean-official/shinify/issues/new) if your modeltype is missing 💪

| modeltype  | name of algorithm      |
| ---------- | ---------------------- |
| `dt_party` | Decision Tree Party    |
| `dr_rpart` | Decision Tree rpart    |
| `knn`      | k Nearest Neighbors    |
| `lin_reg`  | Linear Regression      |
| `log_reg`  | Logistic Regression    |
| `svm`      | Support Vector Machine |
| `rf`       | Random Forest          |

Here are some examples how to call the `shinify` function:

```r
# just call shinify with a simple model
shinify(model)

# call shinify with a log_reg modeltype and the title "awesome discovery" in the shiny app
shinify(model, modeltype="log_reg", "awesome discovery")

# call shinify with a svm modeltype and labels for input and outputs
shinify(model, modeltype="svm", "your awesome title", c("output", "input 1", "input 2"))
```

## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the project
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Make your changes
4. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
5. Push to the branch (`git push origin feature/AmazingFeature`)
6. Open a pull request

## License

Distributed under the MIT License. See `LICENSE` for more information.

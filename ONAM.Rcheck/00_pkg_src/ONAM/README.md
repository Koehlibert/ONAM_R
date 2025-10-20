# Orthogonal neural additive models for interpretable machine learning by functional decomposition of black-box models into explainable predictor effects

## Install package:
``` r
install.packages("devtools")  
library(devtools)  
devtools::install_github("Koehlibert/ONAM_R")
```
If this is the first time using keras or tensorflow in R, you need to run `keras::install_keras()`.
This readme will guide you through the workflow of specifying, fitting and analysing an orthogonal neural additive model.
## Simulate example data

``` r
# Create training data
n <- 1000
x1 <- runif(n, -2, 2)
x2 <- runif(n, -2, 2)
x3 <- runif(n, -2, 2)
noise <- rnorm(n, 0, 1)
y <- sin(x1) + dt(x2, 1) * 4 + 2 * x3 +
  x1 * x2 + noise
train <- cbind(x1, x2, x3, y)
```

## Specify model architecture(s)
Different feature (interaction) effects can have different model architectures, varying in type and complexity. These architectures are to be specified in the `list_of_deep_models`-argument.

``` r
simple_model <- function(inputs) {
  outputs <- inputs %>%
    keras::layer_dense(units = 128, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 64, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 32, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 16, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 8, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 1, activation = "linear", use_bias = TRUE)
  keras::keras_model(inputs, outputs)
}
complex_model <- function(inputs) {
  outputs <- inputs %>%
    keras::layer_dense(units = 512, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 256, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 128, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 64, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 32, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 16, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 8, activation = "relu", use_bias = TRUE) %>%
    keras::layer_dense(units = 1, activation = "linear", use_bias = TRUE)
  keras::keras_model(inputs, outputs)
}
list_of_deep_models = 
  list(simple = simple_model,
       complex = complex_model)
```

## Specify effects of interest
The effects to be fitted are supplied in a `formula`-object. The names of the functions for each feature have to correspond to the names in the `list_of_deep_models`-argument.

``` r
model_formula = y ~ simple(x1) + simple(x2) + simple(x3) + complex(x1, x2) + complex(x1, x2, x3)
```

##Fit onam model
An onam model can then be fitted according to the model formula. An ensembling strategy is used, here with 2 ensemble members. Further model fitting parameters can be specified, such as verbosity, callbacks or number of training steps.
``` r
mod <- onam(formula = model_formula, list_of_deep_models = list_of_deep_models,
            data = train, n_ensemble = 2, epochs = 100, progresstext = FALSE, 
            verbose = 1)
```

## Model evaluation
The fitted model can be investigated regarding goodness of fit and degree of interpretability through the `summary()`-function.
``` r
summary(mod)
```

Fitted feature effects can be visualized in `ggplot`-figures:
``` r
plot_main_effect(mod, "x1")
plot_inter_effect(mod, "x1", "x2", interpolate = TRUE)
```

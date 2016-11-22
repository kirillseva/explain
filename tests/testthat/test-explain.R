library(datasets)

context("explain")

dataset <- iris
dataset$Species <- NULL
dataset$noise_1 <- rnorm(NROW(dataset))
dataset$noise_2 <- rnorm(NROW(dataset))
dataset$noise_3 <- rnorm(NROW(dataset), 100, 10)

describe("end to end", {
  variables <- c("Sepal.Length", "Sepal.Width", "Petal.Width", "noise_1", "noise_2", "noise_3")
  variable_summaries <- lapply(variables, function(x) {
    list(
      mean = mean(dataset[[x]], na.rm = TRUE),
      sd   = sd(dataset[[x]], na.rm = TRUE)
    )
  }) %>% `names<-`(., variables)

  test_that("It can explain a simple model", {
    simple_model <- lm(Petal.Length ~ ., dataset)
    original_coef <- coef(simple_model)
    classifier <- list(
      predict = function(x, ...) { predict(simple_model, x) }
    )
    explanation <- explain(classifier, dataset[42, ], variable_summaries, num_features = 3, verbose = FALSE,
                        num_samples = 10, explainer = MASS::lm.ridge, feature_selection = 'auto')
    expect_is(explanation, "explanation")
    expect_true(setequal(explanation$features, head(variables, 3)))
    expect_true(abs(explanation$intercept) < 1)
    cat('\n'); print(explanation$model)
  })
})

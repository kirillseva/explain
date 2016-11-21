#' Generate an explanation for a classifier's prediction.
#'
#' @param classifier tundraContainer. Can be either a \code{tundraContainer} or
#'   a \code{list} with a \code{predict} method attached.
#' @param onerow data.frame. A one-row dataframe around which we want to get a
#'   local approximation.
#' @param variable_summaries list. A named list that defines means and standard
#'   deviations for the variables that need to be perturbed.
#' @return an explanation object.
#' @export
explain <- function(classifier, onerow, variable_summaries, num_features = 3,
                    num_samples = 100, explainer = MASS::lm.ridge, feature_selection = 'auto', ...) {
  `validate_variable_summaries!`(variable_summaries)
  kernel_width <- 0.75 * sqrt(length(variable_summaries))

  traindf <- lapply(names(variable_summaries), function(name) {
    distribution <- variable_summaries[[name]]
    lapply(seq_len(num_samples), function(i) {
      replacement <- rnorm(1, distribution$mean, distribution$sd)
      within(onerow, eval(bquote({ .(name) = replacement })))
    }) %>% dplyr::bind_rows(.)
  }) %>% dplyr::bind_rows(.) %>% rbind(onerow, .)
  # Allow tundracontainers to predict on train data
  traindf[['explain_scores_']] <- classifier$predict(traindf, list(on_train = TRUE))
  # get weights and important features
  traindf <- traindf[c('explain_scores_', names(variable_summaries))]
  weights <- stats::dist(traindf) %>% as.matrix %>% { .[1,] } %>% kernel(., kernel_width)
  features <- select_features(traindf, weights, num_features, feature_selection)
  traindf <- traindf[c('explain_scores_', features)]
  # generate easy explainable model
  easy_model <- explainer(explain_scores_ ~ ., traindf,
                          weights = weights, na.action = na.omit, ...)
  as.explanation(list(
      intercept   = coef(easy_model)[1]
    , model       = easy_model
    , features    = features
  ))
}

kernel <- function(x, kernel_width) {
  sqrt(exp(-(x * x) / (kernel_width ^ 2)))
}

select_features <- function(traindf, weights, num_features, feature_selection) {
  match.arg(feature_selection, c('auto', 'forward', 'none', 'highest_weights'))
  get(paste0("select_features_", feature_selection))(traindf, weights, num_features)
}

select_features_auto <- function(traindf, weights, num_features) {
  select_features_highest_weights(traindf, weights, num_features)
}

select_features_highest_weights <- function(traindf, weights, num_features) {
  easy_model <- MASS::lm.ridge(explain_scores_ ~ ., traindf, weights = weights, na.action = na.omit)
  coefs <- stats::coef(easy_model)[-1] # remove intercept
  coefs %>% abs %>% sort(., decreasing = TRUE) %>% head(., num_features) %>% names
}

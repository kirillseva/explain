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
explain <- function(classifier, onerow, variable_summaries, num_features = 3, verbose = TRUE,
                    num_samples = 100, explainer = MASS::lm.ridge, feature_selection = 'auto', ...) {
  `validate_variable_summaries!`(variable_summaries)
  variables <- intersect(names(variable_summaries), names(onerow))
  variables <- variables[!is.na(onerow[variables])]
  variable_summaries %<>% .[variables]
  kernel_width <- 0.75 * sqrt(length(variable_summaries))

  if (isTRUE(verbose)) {
    message('Sampling around your data point...')
    lmethod <- pbapply::pblapply
  } else { lmethod <- lapply }
  traindf <- lmethod(variables, function(name) {
    distribution <- variable_summaries[[name]]
    lapply(seq_len(num_samples), function(i) {
      replacement <- rnorm(1, onerow[[name]], distribution$sd)
      within(onerow, eval(bquote({ .(name) = replacement })))
    }) %>% dplyr::bind_rows(.)
  }) %>% dplyr::bind_rows(.) %>% dplyr::bind_rows(onerow, .)
  # Allow tundracontainers to predict on train data
  traindf[['explain_scores_']] <- classifier$predict(traindf, list(on_train = TRUE)) * 100
  # get weights and important features
  traindf <- traindf[c('explain_scores_', names(variable_summaries))]
  # normalize
  lapply(names(variable_summaries), function(name) {
    distribution <- variable_summaries[[name]]
    traindf[[name]] <<- (as.numeric(traindf[[name]]) - distribution$mean) / distribution$sd
  })
  print(head(traindf))
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

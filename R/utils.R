`%||%` <- function(x, y) { if (is.null(x)) { y } else { x } }

#' @export
is.explanation <- function(x) { is(x, "explanation") }
#' @export
as.explanation <- function(x) { `class<-`(x, c("explanation", class(x))) }

`validate_variable_summaries!` <- function(x) {
  stopifnot(length(x) == length(names(x)))
  stopifnot(!any(is.na(names(x))))
  has_mean_and_sd <- sapply(seq_along(x), function(idx) {
    "mean" %in% names(x[[idx]])   &&
    "sd" %in% names(x[[idx]])     &&
    is.numeric(x[[idx]]$mean)     &&
    is.numeric(x[[idx]]$sd)
  })
  stopifnot(all(has_mean_and_sd))
}

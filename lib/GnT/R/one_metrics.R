#' Calculate metrics for a regression model
#' @export
one_metrics <- function(vec_actual, vec_pred) {
  vec_actual %>%
    bind_cols(pred = vec_pred) %>%
    yardstick::metrics(truth=target, estimate=.pred) %>%
    select(-.estimator)
}

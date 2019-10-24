#' Transform proportion to Tukey's Folded Log
#' converts from proportion to odds-like continuous scale.
#' Useful for non-bounded parameters.
#' Presented as curtailed flog, where -Inf and Inf
#' (corresponding to 0 and 1) are replaced with the
#' minimum real value - interquartile range and
#' maximum real value + interquartile range respectively
#' @param p numeric vector in range (0, 1)
#' @return numeric vector
#' @export
#' @importFrom stats IQR
#' @references http://www.sumsar.net/blog/2013/09/going-to-plot-some-proportions/
#' @examples
#' flog(p = c(0, 0.017, 0.029, 0.036, 0.045, 0.531, 1))
flog <- function(p) {
  if (any(p > 1 | p < 0)) {
    warning("p expected in range (0, 1)")
  }
  fl <- (1/2 * log(p) - 1/2 * log(1 - p))
  # modify flog for finite values
  nf <- !is.finite(fl)
  if (any(nf)) {
    pad <- min(c(1, IQR(fl[!nf])), na.rm = TRUE)
    fl[nf & fl < 0] <- min(c(0, fl[!nf]), na.rm = TRUE) - pad
    fl[nf & fl > 0] <- max(c(0, fl[!nf]), na.rm = TRUE) + pad
  }
  fl
}

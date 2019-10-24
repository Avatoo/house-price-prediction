#' Smooth out negative values
#'
#' Spread negative values through other values with sum remaining the same
#'
#' @param vec A numeric vector with negative elements
#' @return vector
#' @export
#' @examples
#' \dontrun{
#' smooth_negative(vec = c(1:24, -50, rep(10, 5), -100))
#' smooth_negative(vec = c(0,72.36,0,0,0))
#' }
smooth_negative <- function(vec = NA) {
  idx = which(vec < 0) # find index of the negative value

  if (sum(vec, na.rm = TRUE) != sum(vec[idx])) {
    # How to deal with only negative values? e.g. c(0,72.36,0,0,0) France, Seebri, 2014-05-02
    q = if (sum(vec, na.rm = TRUE)==0) 0 else sum(vec, na.rm = TRUE)/(sum(vec, na.rm = TRUE) - sum(vec[idx]))
    vec2 = vec * q

    vec2[idx] = 0
  } else {
    vec2 = vec
  }

  stopifnot(all.equal(sum(vec2, na.rm = TRUE),
                      sum(vec, na.rm = TRUE)))
  vec2
}

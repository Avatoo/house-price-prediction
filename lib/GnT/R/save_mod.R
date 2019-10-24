#' Save model fit and metric scores in rds
#' @export
save_mod <- function(fit, scores, outpath, modname) {
  saveRDS(dplyr::lst(fit, scores), file.path(outpath, stringr::str_c(modname, ".rds")))
  return(invisible())
}

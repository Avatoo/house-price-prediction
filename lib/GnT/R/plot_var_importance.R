#' Bar plot for variable importance in tree based model
#' @export
plot_var_importance <- function(fit) {
  data_plot <- data.frame(fit$fit$importance, row.names = row.names(fit$fit$importance)) %>%
    rownames_to_column() %>%
    arrange(desc(IncNodePurity))

  data_plot %>%
    hchart(., type = "bar", color = '#FFC100',
           hcaes(x = rowname,
                 y = IncNodePurity)) %>%
    hc_title(text = 'Variable Importance') %>%
    hc_tooltip(crosshairs = TRUE) %>%
    hc_size(550, 400) %>%
    hc_add_theme(hc_theme_elementary())
}

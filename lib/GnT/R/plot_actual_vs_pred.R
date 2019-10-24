#' Scatter plot comparing actuals and predictions
#' @export
plot_actual_vs_pred <- function(data, xvar, yvar) {
  highchart() %>%
    hc_add_series(data = data, mapping = hcaes(x = !!xvar, y = !!yvar), type = "scatter", color = "#0C1938", alpha=0.5,
                  marker = list(radius = 3), showInLegend = F) %>%
    hc_xAxis(min = 0, title = list(text = "Actuals")) %>%
    hc_yAxis(min = 0, title = list(text = "Predictions")) %>%
    hc_title(text = 'Actuals VS Predictions') %>%
    #hc_tooltip(crosshairs = TRUE) %>%
    hc_tooltip(headerFormat = "<b>Actual: {point.x}, Prediction: {point.y}</b><br>",
               pointFormat = "crim :{point.crim}<br> zn: {point.zn} <br>
                              indus: {point.indus} <br> chas: {point.chas} <br>
                              nox: {point.nox} <br> rm: {point.rm} <br>
                              age: {point.age} <br> dis: {point.dis} <br>
                              rad: {point.rad} <br> tax: {point.tax} <br>
                              ptratio: {point.ptratio} <br> b: {point.b} <br>
                              lstat: {point.lstat}") %>%
    hc_size(550, 500) %>%
    hc_add_theme(hc_theme_elementary())
}

# Acknowledgement https://rpubs.com/adymimos/176226
#' Heatmap for correlation in highcharter style
#' @export
hchart_cor <- function(object, ...) {

  df <- as.data.frame(object)
  is.num <- sapply(df, is.numeric)
  df[is.num] <- lapply(df[is.num], round, 2)
  dist <- NULL

  x <- y <- names(df)

  df <- tbl_df(cbind(x = y, df)) %>%
    gather(y, dist, -x) %>%
    mutate(x = as.character(x),
           y = as.character(y)) %>%
    left_join(data_frame(x = y,
                         xid = seq(length(y)) - 1), by = "x") %>%
    left_join(data_frame(y = y,
                         yid = seq(length(y)) - 1), by = "y")

  ds <- df %>%
    select("xid", "yid", "dist") %>%
    list_parse2()

  fntltp <- JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                         this.series.yAxis.categories[this.point.y] + ': <b>' +
                         Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
  cor_colr <- list( list(0,   '#0C1938'),
                    list(0.5, '#FFFFFF'),
                    list(1,   '#FF7400')
  )
  highchart() %>%
    hc_chart(type = "heatmap") %>%
    hc_xAxis(categories = y, title = NULL) %>%
    hc_yAxis(categories = y, title = NULL) %>%
    hc_add_series(data = ds) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      )) %>%
    hc_tooltip(formatter = fntltp) %>%
    hc_legend(align = "right", layout = "vertical",
              margin = 0, verticalAlign = "top",
              y = 25, symbolHeight = 280) %>%
    hc_colorAxis(stops= cor_colr,min=-1,max=1) %>%
    hc_add_theme(hc_theme_elementary())
}

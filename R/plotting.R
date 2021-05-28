#' Plot echarts timeseries of tweet volumne
#'
#' @param data
#' @param time_column
#' @param n_column
#'
#' @return
#' @export
#'
#' @examples
plot_tweet_volume <- function(data) {
  this_month <- lubridate::floor_date(today(), "month")
  
  data %>%
    echarts4r::e_charts(time) %>%
    echarts4r::e_line(n,
                      name = "# of tweets",
                      smooth = TRUE,
                      legend = FALSE) %>%
    echarts4r::e_x_axis(
      type = "time",
      formatter = htmlwidgets::JS(
        "function(value){
        let date = new Date(value);

        label = `${date.getDate()}-${(parseInt(date.getMonth()) + 1)}-${date.getFullYear()}`;
        return label;
      }"
      )
    ) %>%
    echarts4r::e_axis_labels(y = "Tweets") %>%
    echarts4r::e_theme("westeros") %>%
    echarts4r::e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS(
        "
    function(params) {
      let date = new Date(params[0].value[0])
      let options = { year: 'numeric', month: 'short', day: 'numeric', hour: 'numeric'}
      let title = `<strong>${date.toLocaleDateString('en-US', options=options)}</strong>`
      let num = `${params[0].value[1]} tweets`
      return(`${title}</br>${num}`);
    }"
      )
    ) %>%
    echarts4r::e_datazoom(type = "slider") %>%
    echarts4r::e_zoom(dataZoomIndex = 0,
                      start = 70,
                      end = 100) %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = today() - 7,
      endValue = today(),
      btn = "weekBtn"
    ) %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = this_month,
      endValue = today(),
      btn = "monthBtn"
    ) %>%
    echarts4r::e_button(id = "weekBtn",
                        position = "top",
                        class = "btn btn-primary btn-sm",
                        "This Week") %>%
    echarts4r::e_button(id = "monthBtn",
                        position = "top",
                        class = "btn btn-primary btn-sm",
                        "This Month")
}

#' Plot dataset accumulated by hour
#'
#' @param tweet_dataset Tweet dataset in the format of the rtweet package
#'
#' @return
#' @export
#'
#' @examples
plot_tweet_by_hour <- function(tweet_dataset) {
  tweet_dataset %>% 
    dplyr::group_by(hour = hour(created_at)) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    echarts4r::e_charts(hour) %>%
    echarts4r::e_step(count, name = "Tweets", step = "middle", legend = FALSE) %>%
    echarts4r::e_x_axis(
      min = 0,
      max = 23,
    ) %>%
    echarts4r::e_axis_labels(x = "Time of Day (UTC)", y = "Tweets") %>%
    echarts4r::e_theme("westeros") %>%
    echarts4r::e_tooltip(trigger = "axis", formatter = htmlwidgets::JS("
    function(params) {
      let title = `<strong>${params[0].value[0]}h</strong>`
      let num = `${params[0].value[1]} tweets`
      return(`${title}</br>${num}`);
    }"))
}
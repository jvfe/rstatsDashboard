#' Get number of unique occurrences tweet based on a column
#' 
#' This is useful for creating value boxes.
#'
#' @param data 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
get_unique_value <- function(data, col) {
  col <- enquo(col)
  data %>%
    pull(!!col) %>%
    unique() %>%
    length()
}


#' Get code for embedding a tweet
#'
#' @param user 
#' @param status_id 
#'
#' @return
#' @export
#'
#' @examples
get_tweet_embed <- function(user, status_id) {
  
  url <- stringr::str_glue("https://publish.twitter.com/oembed?url=https://twitter.com/{user}/status/{status_id}&partner=&hide_thread=false")

  
  response <- httr::GET(url) %>%
    httr::content()

  return(shiny::HTML(response$html))
}
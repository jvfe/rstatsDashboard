#' Get and update Tweet dataset
#'
#' @param hashtag The hashtag the tweets must have.
#' @param n The number of tweets to gather.
#' @param filename The filename to write the contents to.
#' @param appname The name of the App
#'    as described in (\url{https://developer.twitter.com/en/portal/projects-and-apps}).
#'    This is required for authentication.
#' @param ... Any other parameters to pass to rtweet::search_tweets
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @examples
getData <- function(hashtag, n, filename, appname, ...) {
  logger::log_threshold(logger::INFO)
  
  logger::log_info("Loading API keys and previous data")
  
  try(dotenv::load_dot_env(), silent = TRUE)
  
  secrets <-
    Sys.getenv(c(
      "TWITTER_KEY",
      "TWITTER_SECRET",
      "ACCESS_TOKEN",
      "ACCESS_SECRET"
    ))
  
  token <- rtweet::create_token(
    app = appname,
    consumer_key = secrets["TWITTER_KEY"],
    consumer_secret = secrets["TWITTER_SECRET"],
    access_token = secrets["ACCESS_TOKEN"],
    access_secret = secrets["ACCESS_SECRET"]
  )
  
  logger::log_info("Getting latest tweets for {hashtag}")
  
  latest_3000 <- rtweet::search_tweets(hashtag,
                                       n = n, ...) %>%
    rtweet::flatten()
  
  if (file.exists(filename)) {
    current_tweet_data <- rtweet::read_twitter_csv(filename)
    all_data <- current_tweet_data %>%
      dplyr::mutate(
        created_at = lubridate::as_datetime(created_at),
        quoted_created_at = lubridate::as_datetime(quoted_created_at),
        account_created_at = lubridate::as_datetime(account_created_at)
      ) %>%
      dplyr::bind_rows(latest_3000) %>%
      clean_data(.)
  } else {
    all_data <- latest_3000 %>%
      dplyr::mutate(
        created_at = lubridate::as_datetime(created_at),
        quoted_created_at = lubridate::as_datetime(quoted_created_at),
        account_created_at = lubridate::as_datetime(account_created_at)
      ) %>%
      clean_data(.)
  }
  
  logger::log_info("Updating dataset")
  
  all_data <- prepend_ids(rtweet::flatten(all_data))
  
  readr::write_csv(all_data,
                   filename,
                   na = "")

}

clean_data <- function(dataset) {
  dataset %>%
    # TODO: Improve this.
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::na_if(., stringr::str_match_all(., "[NA NA]+"))
    )) %>%
    dplyr::distinct(created_at, user_id, text, .keep_all = TRUE) %>%
    janitor::remove_empty(which = "cols")

}

# Functions from the rtweet package to clean IDs
#License: MIT
#YEAR: 2016
#COPYRIGHT HOLDER: Michael W. Kearney
prepend_ids <- function(x) {
  ids <- grepl("\\_id$", names(x))
  x[ids] <- lapply(x[ids], x_ids)
  x
}

x_ids <- function(x) {
  if (is.recursive(x)) {
    x <- lapply(x, function(.)
      ifelse(
        length(.) == 0 || (length(.) == 1 && is.na(.)),
        list(NA_character_),
        list(paste0("x", .))
      ))
    x <- lapply(x, unlist, recursive = FALSE)
  } else {
    x[x == ""] <- NA_character_
    x[!is.na(x)] <- paste0("x", x[!is.na(x)])
    x[!is.na(x)] <- gsub(" ", " x", x[!is.na(x)])
  }
  x
}

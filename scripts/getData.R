library(magrittr)
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
#'
#' @examples
getData <- function(hashtag, n, filename, appname, ...) {
  logger::log_threshold(logger::INFO)
  
  logger::log_info("Loading API keys and previous data")
  
  dotenv::load_dot_env()
  
  secrets <-
    Sys.getenv(c(
      "TWITTER_KEY",
      "TWITTER_SECRET",
      "ACCESS_TOKEN",
      "ACCESS_SECRET"
    ))
  
  current_tweet_data <- rtweet::read_twitter_csv(filename)
  
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
  
  all_data <- current_tweet_data %>%
    dplyr::mutate(
      created_at = lubridate::as_datetime(created_at),
      quoted_created_at = lubridate::as_datetime(quoted_created_at),
      account_created_at = lubridate::as_datetime(account_created_at)
    ) %>%
    dplyr::bind_rows(latest_3000) %>%
    # TODO: Improve this.
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::na_if(., stringr::str_match_all(., "[NA NA]+"))
    )) %>%
    dplyr::distinct(created_at, user_id, text, .keep_all = TRUE) %>%
    janitor::remove_empty(which = "cols")
  
  logger::log_info("Updating dataset")
  
  all_data %>%
    rtweet::write_as_csv(file_name = filename)
  
}

#### Function call ####

getData("#rstats",
        3000,
        "data/rstats_tweets.csv",
        "rtweet-exploration",
        include_rts = FALSE)

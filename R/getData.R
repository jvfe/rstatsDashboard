library(dplyr)
library(lubridate)
library(rtweet)
library(dotenv)

secrets <- Sys.getenv(c("TWITTER_KEY", "TWITTER_SECRET", "ACCESS_TOKEN", "ACCESS_SECRET"))
current_date <- date(Sys.Date())
current_tweet_data <- read_twitter_csv("data/rstats_tweets.csv")

token <- create_token(
  app = "rtweet-exploration",
  consumer_key = secrets["TWITTER_KEY"],
  consumer_secret = secrets["TWITTER_SECRET"],
  access_token = secrets["ACCESS_TOKEN"],
  access_secret = secrets["ACCESS_SECRET"]
)

latest_3000 <- search_tweets(
  "#rstats",
  n = 3000, include_rts = FALSE,
) %>%
  flatten()

all_data <- current_tweet_data %>%
  mutate(
    created_at = as_datetime(created_at),
    quoted_created_at = as_datetime(quoted_created_at),
    account_created_at = as_datetime(account_created_at)
  ) %>%
  bind_rows(latest_3000) %>%
  distinct(created_at, user_id, text, .keep_all = TRUE)

all_data %>%
  write_as_csv(file_name = "data/rstats_tweets")

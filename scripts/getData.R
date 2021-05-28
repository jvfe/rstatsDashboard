library(magrittr)
devtools::load_all()
#### Function call ####

getData("#rstats",
        3000,
        "data/rstats_tweets.csv",
        "rtweet-exploration",
        include_rts = FALSE)

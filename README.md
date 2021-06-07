# rstatsDashboard

This is a self-updating dashboard that shows the latest tweets from the #rstats twitter hashtag. It updates both the data and the dashboard daily using GH Actions, which can also be triggered manually.

[See it live here](https://jvfe.github.io/rstatsdashboard/)

## Repository structure

- `R`: Functions used to build the dashboard and retrieve the data.
- `data`: Contains the data used in the dashboard.
- `scripts`: Scripts that run in the GH Actions CI.
- `styles`: CSS styling for the dashboard.
- `index.Rmd`: Source code for the dashboard.

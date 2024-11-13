## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact:
##
##' *INFO*:
## 1) If a slack token is found in the environment, the slackr_setup function is called.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## slackr_setup()
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!is.null(Sys.getenv("SLACK_TOKEN"))) {
  slackr::slackr_setup(
    channel = "#canvas",
    username = Sys.getenv("SLACK_BOT"),
    icon_emoji = "",
    incoming_webhook_url = Sys.getenv("SLACK_WEBHOOK"),
    token = Sys.getenv("SLACK_TOKEN"),
    config_file = "~/.slackr",
    echo = FALSE
  )
} else {
  message("No Slack token found")
}

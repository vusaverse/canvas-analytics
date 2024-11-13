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

  ## define a function to prepare and send a summary report to Slack

  prepare_and_send_summary <- function(main_data, previous_data, rows_to_process) {
    # Prepare summary report
    current_script <- basename(this.path::sys.path())

    total_observations <- nrow(main_data)
    previous_observations <- nrow(previous_data)
    new_observations <- total_observations - previous_observations

    summary_text <- paste("Script Summary:", current_script, "\n",
                          "- Rows to process:", rows_to_process, "\n",
                          "- Total observations:", total_observations, "\n",
                          "- Previous observations:", previous_observations, "\n",
                          "- New observations:", new_observations, "\n")

    # Send summary report to Slack
    slackr::slackr_msg(summary_text)

    # Return the summary text (optional, in case you want to use it elsewhere)
    return(summary_text)
  }
} else {
  message("No Slack token found")
}

variable merge_bot {
  description = "The executable for running the merge bot."
  default = "artifacts/merge-bot"
}

variable app_id {
  description = "The GitHub App ID for the merge bot."
}

variable webhook_secret {
  description = "The webhook secret set for the merge bot."
}

variable private_key {
  description = "The private key for the merge bot."
  default = "artifacts/github-app.pem"
}

variable user_agent {
  description = "The user agent to use when the merge bot talks with the GitHub API."
  default = "LeapYear/merge-bot"
}

variable "mergit_exe" {
  description = "The executable for running the mergit."
  default     = "artifacts/mergit"
}

variable "app_id" {
  description = "The GitHub App ID for mergit."
}

variable "client_id" {
  description = "The GitHub Client ID for mergit."
}

variable "client_secret" {
  description = "The GitHub Client secret for mergit."
}

variable "webhook_secret" {
  description = "The webhook secret set for mergit."
}

variable "private_key" {
  description = "The private key for mergit."
  default     = "artifacts/github-app.pem"
}

variable "user_agent" {
  description = "The user agent to use when mergit talks with the GitHub API."
  default     = "LeapYear/mergit"
}

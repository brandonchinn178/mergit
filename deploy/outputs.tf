output "ip" {
  value = aws_instance.merge_bot.public_ip
}

output "keyfile" {
  value = module.keypair.keyfile
}

output "url" {
  value = module.domain.url
}

# SSH command without the key file
output "ssh_cmd" {
  value = "ssh ${local.ami_user}@${aws_instance.merge_bot.public_ip}"
}


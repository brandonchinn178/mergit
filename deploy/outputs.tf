output "ip" {
  value = aws_instance.mergit.public_ip
}

output "url" {
  value = module.domain.url
}

output "ssh_cmd" {
  value = "ssh -i ${module.keypair.keyfile} ${local.ami_user}@${aws_instance.mergit.public_ip}"
}

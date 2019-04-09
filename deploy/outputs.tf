output "ip" {
  value = "${aws_instance.merge_bot.public_ip}"
}

output "keyfile" {
  value = "${module.keypair.keyfile}"
}

output "url" {
  value = "${aws_instance.merge_bot.public_dns}"
}

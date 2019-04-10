output "ip" {
  value = "${aws_instance.merge_bot.public_ip}"
}

output "keyfile" {
  value = "${module.keypair.keyfile}"
}

output "url" {
  value = "${aws_lb.load_balancer.dns_name}"
}

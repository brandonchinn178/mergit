# Maintainer: Brandon Chinn

terraform {
  required_version = "0.11.13"

  backend "s3" {
    bucket         = "leapyear-tfstate"
    key            = "merge-bot/terraform.tfstate"
    region         = "us-east-1"
    dynamodb_table = "leapyear-tfstate"
    encrypt        = true
  }
}

provider "aws" {
  region = "us-east-1"
}

locals {
  tags = {
    Name       = "LY Merge Bot"
    User       = "LeapYear Infrastructure Team"
    Maintainer = "merge-bot"
  }
}

## VPC and subnets ##

data "aws_vpc" "default" {
  default = true
}

data "aws_availability_zones" "available" {}

resource "aws_subnet" "subnets" {
  count = 2

  # `count.index` + 2 because subnet conflicts; LBA-XXXX
  cidr_block        = "${cidrsubnet(data.aws_vpc.default.cidr_block, 8, count.index + 2)}"
  vpc_id            = "${data.aws_vpc.default.id}"
  availability_zone = "${data.aws_availability_zones.available.names[count.index]}"

  tags = "${local.tags}"

  map_public_ip_on_launch = true
}

## Primary EC2 instance ##

locals {
  ami           = "ami-011b3ccf1bd6db744" # RHEL 7.6 in us-east-1
  instance_type = "t2.micro"
  ami_user      = "ec2-user"

  bot_conf_dir     = "/etc/merge-bot.d"
  private_key_name = "github-app.pem"
}

module "keypair" {
  source = "git@github.com:LeapYear/infrastructure//modules/keypair?ref=0afa538f9f2ea5801b65e311a252c7f8aea8d412"
  prefix = "merge-bot"
}

resource "aws_security_group" "merge_bot" {
  name        = "merge_bot_security"
  description = "Merge Bot security group"
  vpc_id      = "${data.aws_vpc.default.id}"
  tags        = "${local.tags}"

  ingress {
    from_port = 22
    to_port   = 22
    protocol  = "tcp"

    # LeapYear Office
    cidr_blocks = ["96.82.102.113/32"]
  }

  ingress {
    from_port       = 3000
    to_port         = 3000
    protocol        = "tcp"
    security_groups = ["${aws_security_group.load_balancer.id}"]
  }

  egress {
    protocol    = "-1"
    from_port   = 0
    to_port     = 0
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_instance" "merge_bot" {
  ami                    = "${local.ami}"
  instance_type          = "${local.instance_type}"
  vpc_security_group_ids = ["${aws_security_group.merge_bot.id}"]
  subnet_id              = "${aws_subnet.subnets.0.id}"
  tags                   = "${local.tags}"

  key_name = "${module.keypair.id}"

  connection {
    user        = "${local.ami_user}"
    private_key = "${module.keypair.private_key_pem}"
  }

  provisioner "file" {
    destination = "~/merge-bot"
    source      = "${var.merge_bot}"
  }

  provisioner "file" {
    destination = "~/${local.private_key_name}"
    source      = "${var.private_key}"
  }

  provisioner "file" {
    destination = "~/env"

    content = <<-EOT
      GITHUB_APP_ID=${var.app_id}
      GITHUB_CLIENT_ID=${var.client_id}
      GITHUB_CLIENT_SECRET=${var.client_secret}
      GITHUB_WEBHOOK_SECRET=${var.webhook_secret}
      GITHUB_PRIVATE_KEY=${local.bot_conf_dir}/${local.private_key_name}
      GITHUB_USER_AGENT=${var.user_agent}
      COOKIE_JWK=${local.bot_conf_dir}/cookie-jwk.pem
      MERGE_BOT_URL=https://merge-bot.build-leapyear.com
    EOT
  }

  provisioner "file" {
    destination = "~/merge-bot.service"
    source      = "${path.module}/merge-bot.service"
  }

  provisioner "remote-exec" {
    inline = [
      # merge-bot executable
      "sudo chmod +x merge-bot",
      "sudo mv merge-bot /usr/local/bin/",

      # configuration
      "sudo mkdir -p ${local.bot_conf_dir}",
      "sudo mv ${local.private_key_name} ${local.bot_conf_dir}",
      "sudo mv env ${local.bot_conf_dir}",
      "sudo openssl genrsa -out ${local.bot_conf_dir}/cookie-jwk.pem 2048",

      # logging
      "sudo mkdir -p /var/log/merge-bot",

      # systemd
      "sudo mv merge-bot.service /usr/lib/systemd/system/",
      "sudo systemctl enable --now merge-bot",
    ]
  }
}

## Load Balancer ##

locals {
  merge_bot_port = 3000
}

resource "aws_security_group" "load_balancer" {
  name        = "merge_bot_lb_security"
  description = "Merge Bot Load Balancer security group"
  vpc_id      = "${data.aws_vpc.default.id}"
  tags        = "${local.tags}"

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    protocol    = "-1"
    from_port   = 0
    to_port     = 0
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_lb" "load_balancer" {
  name               = "merge-bot-lb"
  load_balancer_type = "application"
  subnets            = ["${aws_subnet.subnets.*.id}"]

  security_groups = [
    "${aws_security_group.load_balancer.id}",
  ]

  tags = "${local.tags}"

  enable_cross_zone_load_balancing = false
}

module "domain" {
  source = "git@github.com:LeapYear/infrastructure//modules/domain?ref=021dc88e5228787c440f44624fde913894250cb3"

  domain_name    = "build-leapyear.com"
  subdomain_name = "merge-bot"

  target_dns     = "${aws_lb.load_balancer.dns_name}"
  target_zone_id = "${aws_lb.load_balancer.zone_id}"

  ssl_cert_tags = "${local.tags}"
}

resource "aws_lb_listener" "lb_listener_http" {
  load_balancer_arn = "${aws_lb.load_balancer.arn}"
  port              = 80
  protocol          = "HTTP"

  default_action {
    type = "redirect"

    redirect {
      port        = "443"
      protocol    = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}

resource "aws_lb_listener" "lb_listener_https" {
  load_balancer_arn = "${aws_lb.load_balancer.arn}"
  port              = 443
  protocol          = "HTTPS"
  certificate_arn   = "${module.domain.ssl_cert_arn}"

  default_action {
    type             = "forward"
    target_group_arn = "${aws_lb_target_group.lb_target_group.arn}"
  }
}

resource "aws_lb_target_group" "lb_target_group" {
  name        = "merge-bot-lb-target-group"
  port        = "${local.merge_bot_port}"
  protocol    = "HTTP"
  vpc_id      = "${data.aws_vpc.default.id}"
  target_type = "instance"
  tags        = "${local.tags}"
}

resource "aws_lb_target_group_attachment" "lb_target_group_attachment" {
  target_group_arn = "${aws_lb_target_group.lb_target_group.arn}"
  target_id        = "${aws_instance.merge_bot.id}"
  port             = "${local.merge_bot_port}"
}

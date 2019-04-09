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
  ami           = "ami-011b3ccf1bd6db744" # RHEL 7.6 in us-east-1
  instance_type = "t2.micro"
  ami_user      = "ec2-user"

  tags = {
    Name       = "LY Merge Bot"
    User       = "MergeBot"
    Maintainer = "merge-bot"
  }

  bot_conf_dir     = "/etc/merge-bot.d"
  private_key_name = "github-app.pem"
}

## Imported helpers ##

data "aws_vpc" "main" {
  default = true
}

module "keypair" {
  source = "git@github.com:LeapYear/infrastructure//modules/keypair?ref=0afa538f9f2ea5801b65e311a252c7f8aea8d412"
  prefix = "merge-bot"
}

## Resources ##

resource "aws_security_group" "security_group" {
  name        = "merge_bot_security"
  description = "Merge Bot security group"
  vpc_id      = "${data.aws_vpc.main.id}"
  tags        = "${local.tags}"

  ingress {
    from_port = 22
    to_port   = 22
    protocol  = "tcp"

    # LeapYear Office
    cidr_blocks = ["96.82.102.113/32"]
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

resource "aws_instance" "merge_bot" {
  ami                    = "${local.ami}"
  instance_type          = "${local.instance_type}"
  vpc_security_group_ids = ["${aws_security_group.security_group.id}"]
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
      GITHUB_WEBHOOK_SECRET=${var.webhook_secret}
      GITHUB_PRIVATE_KEY=${local.bot_conf_dir}/${local.private_key_name}
      GITHUB_USER_AGENT=${var.user_agent}
    EOT
  }

  provisioner "file" {
    destination = "~/merge-bot.service"
    source      = "${path.module}/merge-bot.service"
  }

  provisioner "file" {
    destination = "~/nginx.repo"
    source      = "${path.module}/nginx.repo"
  }

  provisioner "file" {
    destination = "~/nginx.conf"
    content = <<-EOT
      server {
        listen 443;
        listen [::]:443;

        server_name ${aws_instance.merge_bot.public_dns};

        location / {
          proxy_pass http://localhost:3000/;
        }
      }
    EOT
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

      # systemd
      "sudo mv merge-bot.service /usr/lib/systemd/system/",
      "sudo systemctl enable --now merge-bot",

      # nginx
      "sudo mv nginx.repo /etc/yum.repos.d/",
      "sudo yum install -y nginx",
      "sudo mv nginx.conf /etc/nginx/conf.d/default.conf",
      "sudo setenforce 0", # https://unix.stackexchange.com/questions/218747/nginx-says-open-etc-nginx-conf-d-foo-conf-failed-13-permission-denied
      "sudo systemctl enable --now nginx",
    ]
  }
}

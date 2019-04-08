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
  ami = "ami-011b3ccf1bd6db744" # RHEL 7.6 in us-east-1
  instance_type = "t2.micro"
  ami_user = "ec2-user"
  tags = {
    Name = "LY Merge Bot"
    User = "MergeBot"
    Maintainer = "merge-bot"
  }
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
  name = "merge_bot_security"
  description = "Merge Bot security group"
  vpc_id = "${data.aws_vpc.main.id}"
  tags = "${local.tags}"

  ingress {
    from_port = 22
    to_port = 22
    protocol = "tcp"
    # LeapYear Office
    cidr_blocks = ["96.82.102.113/32"]
  }

  ingress {
    from_port = 443
    to_port = 443
    protocol = "tcp"
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
  ami = "${local.ami}"
  instance_type = "${local.instance_type}"
  vpc_security_group_ids = ["${aws_security_group.security_group.id}"]
  tags = "${local.tags}"

  key_name = "${module.keypair.id}"
  connection {
    user = "${local.ami_user}"
    private_key = "${module.keypair.private_key_pem}"
    timeout = "2m"
  }
}

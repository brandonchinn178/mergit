# Maintainer: Brandon Chinn

terraform {
  required_version = "0.12.21"

  backend "s3" {
    bucket         = "leapyear-tfstate"
    key            = "mergit/terraform.tfstate"
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
    Name        = "Mergit"
    User        = "LeapYear Infrastructure Team"
    Maintainer  = "mergit"
  }
  extra_tags = merge(local.tags, {
    Customer    = "all"
    Department  = "infra"
    Environment = "prod"
    Owner       = "infra"
  })
}

## VPC and subnets ##

data "aws_vpc" "default" {
  default = true
}

data "aws_availability_zones" "available" {
}

resource "aws_subnet" "subnets" {
  count = 2

  # `count.index` + 2 because subnet conflicts; LBA-XXXX
  cidr_block        = cidrsubnet(data.aws_vpc.default.cidr_block, 8, count.index + 2)
  vpc_id            = data.aws_vpc.default.id
  availability_zone = data.aws_availability_zones.available.names[count.index]

  tags = local.tags

  map_public_ip_on_launch = true
}

## Primary EC2 instance ##

locals {
  ami           = "ami-04505e74c0741db8d" # Ubuntu 20.04 in us-east-1
  instance_type = "t2.micro"
  ami_user      = "ubuntu"

  mergit_conf_dir     = "/etc/mergit.d"
  private_key_name = "github-app.pem"
}

module "keypair" {
  source = "git@github.com:LeapYear/infrastructure//modules/keypair?ref=0afa538f9f2ea5801b65e311a252c7f8aea8d412"
  prefix = "mergit"
}

resource "aws_security_group" "mergit" {
  name        = "mergit_security"
  description = "Mergit security group"
  vpc_id      = data.aws_vpc.default.id
  tags        = local.tags

  ingress {
    from_port = 22
    to_port   = 22
    protocol  = "tcp"

    cidr_blocks = ["96.82.102.113/32", "3.234.201.220/32", "10.0.0.0/8"]
  }

  ingress {
    from_port       = 3000
    to_port         = 3000
    protocol        = "tcp"
    security_groups = [aws_security_group.load_balancer.id]
  }

  egress {
    protocol    = "-1"
    from_port   = 0
    to_port     = 0
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_instance" "mergit" {
  ami                    = local.ami
  instance_type          = local.instance_type
  vpc_security_group_ids = [aws_security_group.mergit.id]
  subnet_id              = aws_subnet.subnets[0].id
  tags                   = merge(local.extra_tags, {"Name": "mergit", "Purpose": "Mergit server"})

  key_name = module.keypair.id

  connection {
    host        = coalesce(self.public_ip, self.private_ip)
    type        = "ssh"
    user        = local.ami_user
    private_key = module.keypair.private_key_pem
  }

}



## Load Balancer ##

locals {
  mergit_port = 3000
}

resource "aws_security_group" "load_balancer" {
  name        = "mergit_lb_security"
  description = "Mergit Load Balancer security group"
  vpc_id      = data.aws_vpc.default.id
  tags        = local.tags

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
  name               = "mergit-lb"
  load_balancer_type = "application"
  subnets            = aws_subnet.subnets.*.id

  security_groups = [
    aws_security_group.load_balancer.id,
  ]


  tags = merge(local.extra_tags, {"Purpose": "Mergit Traefik"})

  enable_cross_zone_load_balancing = false
}

module "domain" {
  source = "git@github.com:LeapYear/infrastructure//modules/domain?ref=9a91f34e3c20faf792372c6c0d32425cf09b0f6a"

  domain_name    = "build-leapyear.com"
  subdomain_name = "mergit"

  target_dns     = aws_lb.load_balancer.dns_name
  target_zone_id = aws_lb.load_balancer.zone_id

  ssl_cert_tags = local.tags
}

resource "aws_lb_listener" "lb_listener_http" {
  load_balancer_arn = aws_lb.load_balancer.arn
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
  load_balancer_arn = aws_lb.load_balancer.arn
  port              = 443
  protocol          = "HTTPS"
  certificate_arn   = module.domain.ssl_cert_arn

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.lb_target_group.arn
  }
}

resource "aws_lb_target_group" "lb_target_group" {
  name        = "mergit-lb-target-group"
  port        = local.mergit_port
  protocol    = "HTTP"
  vpc_id      = data.aws_vpc.default.id
  target_type = "instance"
  tags        = local.tags
}

resource "aws_lb_target_group_attachment" "lb_target_group_attachment" {
  target_group_arn = aws_lb_target_group.lb_target_group.arn
  target_id        = aws_instance.mergit.id
  port             = local.mergit_port
}


## Ansible ##

data "template_file" "ansible_hosts" {
  template = file("${path.module}/ansible_hosts.tpl")
  vars = {
    ami_user         = local.ami_user
    app_id           = var.app_id
    client_id        = var.client_id
    client_secret    = var.client_secret
    ip               = aws_instance.mergit.public_ip
    keyfile          = module.keypair.keyfile
    mergit_conf_dir  = local.mergit_conf_dir
    private_key_name = local.private_key_name
    user_agent       = var.user_agent
    webhook_secret   = var.webhook_secret
  }
}

resource "local_file" "ansible_hosts" {
  content  = data.template_file.ansible_hosts.rendered
  filename = "${path.module}/ansible_hosts"
}

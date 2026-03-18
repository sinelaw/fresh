# Terraform syntax highlighting test
terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = var.region
}

variable "region" {
  type    = string
  default = "us-east-1"
}

resource "aws_instance" "hello" {
  ami           = "ami-12345678"
  instance_type = "t3.micro"

  tags = {
    Name = "hello-world"
  }
}

output "instance_id" {
  value = aws_instance.hello.id
}

${ip} ansible_user=${ami_user} ansible_ssh_private_key_file=${keyfile}

[mergit:vars]
ami_user=${ami_user}
app_id=${app_id}
client_id=${client_id}
client_secret=${client_secret}
mergit_conf_dir=${mergit_conf_dir}
private_key_name=${private_key_name}
user_agent=${user_agent}
webhook_secret=${webhook_secret}

[mergit]
${ip}

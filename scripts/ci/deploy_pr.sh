#! /usr/bin/env bash

if [ $TRAVIS_PULL_REQUEST ]
then
    total_steps=6
    echo "Deploying app for pull request"
    echo "(1/$total_steps) Installing Gigalixir CLI"
    apt-get update
    apt-get install python python-pip
    pip install --user gigalixir

    echo "(2/$total_steps) Logging into gigalixir"
    gigalixir login -e $GIGALIXIR_EMAIL -y -p $GIGALIXIR_PASSWORD

    app_name="$GIGALIXIR_APP_NAME-dev-$TRAVIS_PULL_REQUEST"
    echo "(3/$total_steps) Creating gigalixir app $app_name"
    gigalixir create --name $app_name || gigalixir set_git_remote $app_name

    echo "(4/$total_steps) Setting environment variables" 
    gigalixir set_config $app_name DATABASE_URL $DEV_DATABASE_URL
    gigalixir set_config $app_name ELM_19_BINARY_URL $DEV_ELM_19_BINARY_URL
    gigalixir set_config $app_name PACKAGE_SITE $DEV_PACKAGE_SITE
    gigalixir set_config $app_name SERVER_ORIGIN "https://$app_name.gigalixirapp.com"
    gigalixir set_config $app_name SOCKET_ORIGIN "wss://$app_name.gigalixirapp.com"

    echo "(5/$total_steps) Building on Gigalixir"
    git push -f gigalixir HEAD:refs/heads/master
    
    echo "(6/$total_steps) Running database migrations"
    gigalixir migrate $app_name

    gigalixir set_
fi

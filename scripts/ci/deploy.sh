#! /usr/bin/env bash

while [ ! $# -eq 0 ]
do
    case "$1" in
        --now_token)
            shift;
            if test $# -gt 0; then
                if [[ "$1" =~ ^--.* ]]; then
                    echo "now token not specified with --now_token flag";
                    exit 1;
                fi
                now_token=$1;
            else
                echo "now token not specified with --now_token flag";
                exit 1;
            fi
            shift;
            ;;

        --heroku_token)
            shift;
            if test $# -gt 0; then
                if [[ "$1" =~ ^--.* ]]; then
                    echo "now token not specified with --heroku_token flag";
                    exit 1;
                fi
                heroku_token=$1;
            else
                echo "now token not specified with --heroku_token flag";
                exit 1;
            fi
            shift;
            ;;

        --release_hook)
            shift;
            if test $# -gt 0; then
                if [[ "$1" =~ ^--.* ]]; then
                    echo "release hook not specified with --release_hook flag";
                    exit 1;
                fi
                release_hook=$1;
            else
                echo "release hook not specified with --release_hook flag";
                exit 1;
            fi
            shift;
            ;;

        --branch)
            shift;
            if test $# -gt 0; then
                if [[ "$1" =~ ^--.* ]]; then
                    echo "branch name not specified with --branch flag";
                    exit 1;
                fi
                branch_name=$1;
            else
                echo "branch name not specified with --branch flag";
                exit 1;
            fi
            shift;
            ;;
        
        --commit_hash)
            shift;
            if test $# -gt 0; then
                if [[ "$1" =~ ^--.* ]]; then
                    echo "commit hash not specified with --commit_hash flag";
                    exit 1;
                fi
                commit_hash=$1;
            else
                echo "commit hash not specified with --commit_hash flag";
                exit 1;
            fi
            shift; 
            ;; 
    esac
done

if [[ -z $branch_name ]]; then
    echo "ERROR: --branch is required";
    exit 1;
fi

if [[ $branch_name == "master" ]]; then
    if [[ -z $release_hook ]]; then
        echo "ERROR: --release_hook is required on master deploys";
        exit 1;
    fi

    if [[ -z $commit_hash ]]; then
        echo "ERROR: --commit_hash is required on master deploys";
        exit 1;
    fi

    echo '{ "name": "ellie-production", "alias": "ellie-app.com", "scale": { "bru1": { "min": 0, "max": 0 }, "sfo1": { "min": 1, "max": 1 } } }' > ./now.json

    now -t $now_token \
        -A ./now.json \
        -e SECRET_KEY_BASE=@secret-key-base \
        -e DATABASE_URL=@production-db \
        -e SENTRY_DSN=@sentry-dsn \
        -e SENTRY_API_KEY=@sentry-api-key \
        -e HOSTNAME=https://ellie-app.com \
        -n ellie-production \
        ellie-app/ellie

    now -t $now_token -A ./now.json alias

    curl $release_hook \
        -X POST \
        -H 'Content-Type: application/json' \
        -d '{"version": "'"$commit_hash"'"}'

    rm ./now.json

    # remove deployments that are more than 2 behind
    now -t $now_token ls --all ellie-production | grep DOCKER | awk '{ print $2 }' | tail -n +3 | xargs -I {} sh -c 'echo "y" | now -t '"$now_token"' rm {}'
else
    green='\033[0;32m'

    echo -e "${green}--> CHECKING COMMAND LINE ARGUMENTS"
    if [[ -z $heroku_token ]]; then
        echo "ERROR: --heroku_token is required on branch deploys";
        exit 1;
    fi

    echo -e "${green}--> CONFIGURING NOW CLI"
    app_name=ellie-test-$branch_name
    echo '{ "name": "'"$app_name"'", "alias": "'"$app_name"'.now.sh" }' > ./now.json

    echo -e "${green}--> SETING UP DATABASE ON HEROKU"
    existing_app=$(HEROKU_API_KEY=$heroku_token heroku apps | grep $app_name)
    if [[ -z $existing_app ]]; then
        HEROKU_API_KEY=$heroku_token heroku apps:create --app=$app_name --addons heroku-postgresql:hobby-dev
    fi
    raw_database_url=$(HEROKU_API_KEY=$heroku_token heroku config:get DATABASE_URL --app=$app_name)
    database_url=${raw_database_url//postgres:\/\//"ecto://"}

    echo -e "${green}--> DEPLOYING TO NOW.SH"
    now -t $now_token \
        -A ./now.json \
        -n $app_name \
        -e SECRET_KEY_BASE=@secret-key-base \
        -e DATABASE_URL=$database_url \
        -e SENTRY_DSN=@sentry-dsn \
        -e SENTRY_API_KEY=@sentry-api-key \
        -e HOSTNAME=https://$app_name.now.sh \
        ellie-app/ellie#$branch_name
    now -t $now_token -A ./now.json alias
    rm ./now.json

    echo -e "${green}--> CLEANING UP OLD DEPLOYMENTS"
    now -t $now_token ls --all $app_name | grep DOCKER | awk '{ print $2 }' | tail -n +2 | xargs -I {} sh -c 'echo "y" | now -t '"$now_token"' rm {}'
fi

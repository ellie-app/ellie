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

    now -t $now_token \
        -e SECRET_KEY_BASE=@secret-key-base \
        -e DATABASE_URL=@production-db \
        -e SENTRY_DSN=@sentry-dsn \
        -e SENTRY_API_KEY=@sentry-api-key \
        -e HOSTNAME=ellie-app.com \
        -n ellie-production \
        ellie-app/ellie

    now -t $now_token alias ellie-production ellie-app.com

    curl $release_hook \
        -X POST \
        -H 'Content-Type: application/json' \
        -d '{"version": "$commit_hash"}'
else 
    now -t $now_token \
        -e SECRET_KEY_BASE=@secret-key-base \
        -e DATABASE_URL=@staging-db \
        -e SENTRY_DSN= \
        -e SENTRY_API_KEY= \
        ellie-app/ellie#$branch_name
fi

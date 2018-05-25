#! /usr/bin/env bash

while [ ! $# -eq 0 ]
do
    case "$1" in
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
    esac
done

if [[ -z $branch_name ]]; then
    echo "ERROR: --branch is required";
    exit 1;
fi


if [[ $branch_name == "master" ]]; then
    now -t $NOW_TOKEN \
        -e SECRET_KEY_BASE=@secret-key-base \
        -e DATABASE_URL=@production-db \
        -e SENTRY_DSN=@sentry-dsn \
        -e SENTRY_API_KEY=@sentry-api-key \
        -e HOSTNAME=ellie-app.com \
        -n ellie-production \
        ellie-app/ellie

    now alias ellie-production ellie-app.com
else 
    now -t $NOW_TOKEN \
        -e SECRET_KEY_BASE=@secret-key-base \
        -e DATABASE_URL=@staging-db \
        -e SENTRY_DSN=@sentry-dsn \
        -e SENTRY_API_KEY=@sentry-api-key 
        ellie-app/ellie#$branch_name
fi

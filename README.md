# Ellie

## Development

Ellie is a web application with an Elixir backend and an Elm frontend.
For development we rely on the Mix and Phoenix development cylce tasks.
The development environment requires:

- [Elixir](https://elixir-lang.org/install.html) 1.7
- [PostgreSQL](https://www.postgresql.org/) 10
- [Node](https://nodejs.org) LTS

Provided the local postgres server is running and `createuser -s postgres` has been run, to setup and then start the dev
server, run the following:

```sh
$ make bootstrap
$ make serve
```

## Production

To build the AWS infrastructure that Ellie can be deployed to, run the folling:

> **⚠️ WARNING**
> This does not operate in the Free Tier, running the following script will incur a cost from AWS.

```sh
$ GITHUB_TOKEN=<github token> SSH_KEY_NAME=<ssh key name> scripts/cfn create
```

After this is done, you will need to manually setup the public listener(s) on the load balancer and ensure the webhook
has been correctly configured. The pipeline will now kick off after a new release has been tagged.

The following script can be used to tear down the AWS infrastructure:

```sh
$ GITHUB_TOKEN=<github token> SSH_KEY_NAME=<ssh key name> scripts/cfn destroy
```

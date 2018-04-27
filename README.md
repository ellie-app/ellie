# Ellie

## Development

Ellie is a web application with an Elixir backend and an Elm frontend. It runs inside Docker.
For development we use docker-compose to install all of the software and start up the
various programs in the correct order. To begin developing Ellie, all you need to do is:

1. Install Docker

[Official installation instructions](https://docs.docker.com/install/)

2. Start with docker-compose

```
$ docker-compose up
```

The first time you run this it will download the images for our base operating system, install
all of the software for building and running, and compile everything. It attaches the project
directory as a volume, so all of the build artifacts will be written to your file system. 

> **⚠️ WARNING**
> It's important to be careful about editor tools that modify these build artifacts! They are
> generated on a Linux operating system and might be incompatible with your machine.

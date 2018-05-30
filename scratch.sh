#!/usr/bin/env bash
parameter="postgres://postgres:hi"

stuff=${parameter//postgres:\/\//"ecto://"}
echo $stuff

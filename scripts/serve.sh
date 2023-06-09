#!/usr/bin/env bash

check-install () {
  if ! command -v $1 &> /dev/null
  then
      echo "$1 could not be found; please try: yarn global add $1"
      exit
  fi
}

check-install "live-server"

# Launch the main server
live-server --port=4000 --watch="index.html","main.css","dist/","js/target/scala-3.2.1/scalajs-bundler/main/monet-fastopt.js" --no-browser

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT
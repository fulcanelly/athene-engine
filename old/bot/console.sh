#!/usr/bin/env bash

docker exec -it $(docker ps | grep 'bot' | awk '{ print $1 }') ./console.rb

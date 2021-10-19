#!/bin/sh

docker build -t petrinizer:latest .
docker run -it petrinizer:latest make

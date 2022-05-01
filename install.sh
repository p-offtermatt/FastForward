#!/bin/bash

# install dotnet
wget https://dot.net/v1/dotnet-install.sh
chmod +x dotnet-install.sh
./dotnet-install.sh -c 6.0

# installing psutil
apt-get install python3
apt-get install python3-pip
pip install psutil

# adding nuget sources
dotnet nuget add source --name nuget.org https://api.nuget.org/v3/index.json

# installing wine32 (needed for woflan)
dpkg --add-architecture i386
apt-get update
apt-get install wine32

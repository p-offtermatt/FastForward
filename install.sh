#!/bin/bash

# install dotnet
wget https://dot.net/v1/dotnet-install.sh
sudo chmod +x dotnet-install.sh
./dotnet-install.sh -c 6.0

# adding nuget sources
cd artifact/src/
dotnet nuget add source --name nuget.org https://api.nuget.org/v3/index.json
cd ../..

# installing psutil
sudo apt-get install -y python3
sudo apt-get install -y python3-pip
pip install psutil

# installing wine32 (needed for woflan)
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get install wine32

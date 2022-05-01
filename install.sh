#!/bin/bash

# install dotnet
wget https://packages.microsoft.com/config/ubuntu/21.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

sudo apt-get update; \
  sudo apt-get install -y apt-transport-https && \
  sudo apt-get update && \
  sudo apt-get install -y dotnet-sdk-6.0

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

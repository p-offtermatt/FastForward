#!/bin/bash

# install dotnet
sudo apt-get install dotnet
wget https://packages.microsoft.com/config/ubuntu/21.04/packages-microsoft-prod.deb -O >
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
<get update &&   sudo apt-get install -y dotnet-sdk-6.0

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

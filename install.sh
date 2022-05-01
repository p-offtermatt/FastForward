#!/bin/bash

# install dotnet
snap install dotnet-sdk --classic --channel=6.0

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

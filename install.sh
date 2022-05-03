#!/bin/bash

# install dotnet
wget https://dot.net/v1/dotnet-install.sh
sudo chmod +x dotnet-install.sh
./dotnet-install.sh -c 6.0

# add dotnet to path
echo "PATH=/home/cav2022/.dotnet:$PATH" >> ~/.bashrc

# adding nuget sources
cd artifact/src/
dotnet nuget add source --name nuget.org https://api.nuget.org/v3/index.json
cd ../..

# installing python packages
sudo apt-get install -y python3
sudo apt-get install -y python3-pip
pip install psutil
pip install tabulate
pip install matplotlib

# installing wine32 (needed for woflan)
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get install wine32

#!/bin/bash

# installing psutils
apt-get install psutils

# adding nuget sources
dotnet nuget add source --name nuget.org https://api.nuget.org/v3/index.json

# installing wine32 (needed for woflan)
dpkg --add-architecture i386
apt-get update
apt-get install wine32

#!/bin/bash
rm -R ../benchmark/fastforward/
dotnet publish -r linux-x64 -c Release
mv bin/Release/netcoreapp3.1/linux-x64/publish ../benchmark/tools/fastforward
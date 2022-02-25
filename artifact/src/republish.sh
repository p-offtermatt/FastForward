#!/bin/bash
rm -R ../benchmark/tools/fastforward
dotnet publish -r linux-x64 -c Release /nowarn:CS8509
mv bin/Release/netcoreapp3.1/linux-x64/publish ../benchmark/tools/fastforward

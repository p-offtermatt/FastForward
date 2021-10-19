#!/bin/bash
echo "---- Installing Nuget Packages ---"
mkdir -p $HOME/.nuget/
cp -R dependencies/nuget/packages/ $HOME/.nuget/
echo "---- Installing dependencies/pkgs ----"
sudo dpkg -i dependencies/pkgs/*.deb

echo "---- Making LoLA ----"
cd dependencies/lola
sudo ./configure
sudo make
sudo make install

echo "---- Making MIST ----"
cd ../mist
sudo ./configure
sudo make
sudo make install

echo "---- Installing Python3 packages ----"
cd ../python
python3 -m pip install *.whl

echo "---- Installing Python2 packages ----"
cd ../python2
python2 get-pip.py --no-index --find-links=.
python2 -m pip install *.whl

echo "---- Creating link to z3 binary ----"
cd ../z3
sudo ln -s $PWD/bin/z3 /usr/local/bin/z3

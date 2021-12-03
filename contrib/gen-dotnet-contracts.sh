#!/bin/zsh
set -e
NS="Ariane.SaveFile.Contract"
mkdir $NS
pushd $NS
dotnet new classlib
rm -v *.cs
xscgen -n "=$NS" ../tmlu.xsd

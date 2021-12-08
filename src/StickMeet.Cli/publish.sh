dotnet publish -p:RuntimeIdentifier=linux-x64 -p:PublishSingleFile=true -o foo
dotnet publish -p:PublishSingleFile=true -p:SelfContained -o foo 
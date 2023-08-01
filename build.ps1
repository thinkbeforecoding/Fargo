#! /usr/bin/env pwsh

dotnet tool restore
dotnet build -c Release
dotnet pack -c Release --no-build -o bin/nuget
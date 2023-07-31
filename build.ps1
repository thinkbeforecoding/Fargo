#! /usr/bin/env pwsh

dotnet tool restore
dotnet build -c Release
dotnet paket pack bin/nuget
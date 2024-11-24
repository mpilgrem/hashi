# This PowerShell script is intended to be run from the hashi package directory

# Check that hashi.cabal exists
if (-Not (Test-Path -Path "./hashi.cabal" -PathType Leaf)) {
  Write-Host -ForegroundColor Red -Message @"
This script is intended to be run from the hashi package directory. hashi.cabal does not exist. Aborting ...
"@
  exit
}

# Check that the bin directory does not already exist
if (Test-Path -Path "bin" -PathType Container) {
  Write-Host -ForegroundColor Red -Message @"
The directory bin already exists. Aborting ...
"@
  exit
}

Write-Host -ForegroundColor Green -Message @"
Creating the bin directory...
"@
New-Item -Path "bin" -ItemType Directory

# Build hashi-solver and copying to bin directory
Write-Host -ForegroundColor Green -Message @"

Building hashi-solve executable and copying to bin directory ...

"@
stack --local-bin-path=bin install hashi:exe:hashi-solve

if (-Not (Test-Path -Path "bin/hashi-solve.exe" -PathType Leaf)) {
  Write-Host -ForegroundColor Red -Message @"
bin/hashi-solve.exe does not exist. Aborting ...
"@
  exit
}

# Copy MINGW64 *.dll dependencies to bin directory
Write-Host -ForegroundColor Green -Message @"

Seeking to copy MINGW64 *.dll dependencies to bin directory

"@
stack exec -- ldd bin/hashi-solve.exe |
ForEach-Object {
  if ($_ -Match '^\t(\S+)\s*=> /mingw64.*$') {
    $matches[1]
  }
} |
Sort-Object |
ForEach-Object {
  $output = stack exec -- where.exe $_
  $firstLine = ($output -Split "`r?`n")[0]
  Copy-Item $firstLine -Destination bin
}

Write-Host -ForegroundColor Green -Message @"
Reporting the content of the bin directory...
"@

Get-ChildItem -Path "bin"

Write-Host -ForegroundColor Green -Message @"

Test executable by commanding: bin/hashi-solve.exe
"@

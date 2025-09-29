@echo off 

call build.bat > nul 

pushd build

call lehm.exe

popd build 
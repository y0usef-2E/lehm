@echo off 

if not defined VISUAL_DSA (
	call prepare.bat > nul
)

if not exist build mkdir build

odin build . -debug -out:.\build\lehm.exe
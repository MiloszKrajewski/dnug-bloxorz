@echo off

setlocal
pushd %~dp0
mkdir .\out 2> nul
del .\out /q /s
call :build_html
call :build_dot
popd
endlocal
goto :end

:build_html
call %~dp0\node_modules\.bin\generate-md.cmd --layout github --input src --output out
exit /b

:build_dot
pushd src
for %%f in (*.dot) do (
    echo DOT: %%f
    call %~dp0\node_modules\.bin\diagrams.cmd dot %%f ..\out\%%f.svg
)
popd
exit /b

:end

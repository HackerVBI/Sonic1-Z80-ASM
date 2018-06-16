@ECHO OFF
CLS
COLOR 17

CD %~dp0

IF EXIST S1.o ERASE S1.o
IF EXIST ROM_NEW.sms ERASE ROM_NEW.sms
IF EXIST ROM_NEW.sym ERASE ROM_NEW.sym


ECHO.
ECHO Compiling Object File...
IF %ERRORLEVEL% EQU 0 WLADX\wla-z80 -v s1.sms.asm 

ECHO.
ECHO Compiling start file...
_spg\sjasmplus "sonic.asm"
_spg\sjasmplus "intro.asm"

ECHO.
ECHO Linking ROM...
ECHO ===============================================================================
IF %ERRORLEVEL% EQU 0 wladx\wlalink -r link.txt _spg/ROM_NEW.sms

ECHO.

cd _spg
spgbld.exe -b spgbld.ini sonic.spg -c 0

rem IF %ERRORLEVEL% EQU 0 VBinDiff\VBinDiff.exe ROM.sms ROM_NEW.sms
cd ../
ERASE s1.o
PAUSE
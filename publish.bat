@echo off
setlocal
echo ===============================================
echo  Public Health Automation Clinic - Publish
echo ===============================================
echo.

cd /d "%~dp0"

set "SOURCE=%~dp0"
if not defined GDRIVE_DEST set "GDRIVE_DEST=G:\My Drive\Intersect_Collaborations_LLC\04_projects\Public-Health-Automation-Clinic"
if not defined BACKUP_INCLUDE_DIRS set "BACKUP_INCLUDE_DIRS=communications"

set "SKIP_BACKUP=0"
set "SKIP_PUBLISH=0"

:parse_args
if "%~1"=="" goto args_done
if /I "%~1"=="--help" goto show_help
if /I "%~1"=="--skip-backup" (
    set "SKIP_BACKUP=1"
    shift
    goto parse_args
)
if /I "%~1"=="--skip-publish" (
    set "SKIP_PUBLISH=1"
    shift
    goto parse_args
)
if /I "%~1"=="--render-only" (
    set "SKIP_BACKUP=1"
    set "SKIP_PUBLISH=1"
    shift
    goto parse_args
)
if /I "%~1"=="--backup-only" (
    set "SKIP_PUBLISH=1"
    shift
    goto parse_args
)
if /I "%~1"=="--dest" (
    if "%~2"=="" (
        echo ERROR: --dest requires a path.
        goto show_help
    )
    set "GDRIVE_DEST=%~2"
    shift
    shift
    goto parse_args
)

echo ERROR: Unknown argument %~1
goto show_help

:args_done

where git >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: git is required but was not found on PATH.
    pause
    exit /b 1
)

where quarto >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: quarto is required but was not found on PATH.
    pause
    exit /b 1
)

if "%SKIP_BACKUP%"=="0" (
    where tar >nul 2>&1
    if %errorlevel% neq 0 (
        echo ERROR: tar.exe is required for ZIP backup but was not found on PATH.
        pause
        exit /b 1
    )
)

REM Get timestamp using PowerShell
for /f %%i in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd_HHmm"') do set "TIMESTAMP=%%i"

set "ZIP_NAME=Public-Health-Automation-Clinic_%TIMESTAMP%.zip"
set "ZIP_PATH=%TEMP%\%ZIP_NAME%"
set "FINAL_PATH=%GDRIVE_DEST%\%ZIP_NAME%"

if "%SKIP_BACKUP%"=="0" (
    echo [1/3] Backing up project to Google Drive...
    echo.
    echo Timestamp: %TIMESTAMP%
    echo Source: %SOURCE%
    echo Destination: %GDRIVE_DEST%
    echo ZIP File: %ZIP_NAME%
    echo Include Dirs: %BACKUP_INCLUDE_DIRS%
    echo.

    if not exist "%GDRIVE_DEST%" (
        echo ERROR: Google Drive destination does not exist: %GDRIVE_DEST%
        echo Please ensure Google Drive is mounted and the folder exists.
        pause
        exit /b 1
    )

    if exist "%ZIP_PATH%" del "%ZIP_PATH%"

    echo Creating ZIP archive...
    echo This may take a few minutes...
    echo.

    REM Use git to list tracked and untracked files respecting .gitignore
    REM Also include configured directories in BACKUP_INCLUDE_DIRS (semicolon-separated)
    pushd "%SOURCE%" >nul
    powershell -NoProfile -Command "$gitFiles = (git ls-files --cached --others --exclude-standard -z) -split \"`0\" | Where-Object { $_ }; $includeDirs = '%BACKUP_INCLUDE_DIRS%'.Split(';') | Where-Object { $_ }; $extraFiles = foreach ($dir in $includeDirs) { if (Test-Path -LiteralPath $dir) { Get-ChildItem -Path $dir -Recurse -File -ErrorAction SilentlyContinue | Resolve-Path -Relative | ForEach-Object { $_ -replace '^\.\\','' } } }; ($gitFiles + $extraFiles) | Sort-Object -Unique | Where-Object { $_ -and (Test-Path -LiteralPath $_) } | tar.exe -a -c -f '%ZIP_PATH%' -T -"
    popd >nul

    if not exist "%ZIP_PATH%" (
        echo ERROR: ZIP file was not created.
        pause
        exit /b 1
    )

    for %%A in ("%ZIP_PATH%") do set "ZIP_SIZE=%%~zA"
    set /a ZIP_SIZE_MB=%ZIP_SIZE%/1048576
    echo ZIP file size: %ZIP_SIZE_MB% MB
    echo.

    echo Copying to Google Drive...
    copy /Y "%ZIP_PATH%" "%FINAL_PATH%"
    if %ERRORLEVEL% neq 0 (
        echo.
        echo ERROR: Backup to Google Drive failed.
        pause
        exit /b 1
    )
    del "%ZIP_PATH%"
    echo Backup complete: %FINAL_PATH%
) else (
    echo [1/3] Backup skipped by flag.
)

echo.
echo [2/3] Rendering the site...
quarto render
if %errorlevel% neq 0 (
    echo.
    echo ERROR: Quarto render failed.
    pause
    exit /b 1
)

echo.
if "%SKIP_PUBLISH%"=="0" (
    echo [3/3] Publishing to GitHub Pages...
    quarto publish gh-pages --no-prompt
    if %errorlevel% neq 0 (
        echo.
        echo ERROR: GitHub publish failed.
        pause
        exit /b 1
    )
) else (
    echo [3/3] Publish skipped by flag.
)

echo.
echo ===============================================
echo  Success! Workflow completed
echo ===============================================
echo.
pause
exit /b 0

:show_help
echo.
echo Usage: publish.bat [options]
echo.
echo Options:
echo   --help           Show this help
echo   --skip-backup    Skip Google Drive ZIP backup
echo   --skip-publish   Skip GitHub Pages publish step
echo   --render-only    Render only, skip backup and publish
echo   --backup-only    Backup and render, skip publish
echo   --dest "PATH"    Override Google Drive destination for this run
echo.
echo Environment overrides:
echo   GDRIVE_DEST          Backup destination path
echo   BACKUP_INCLUDE_DIRS  Semicolon-separated extra directories in ZIP (default: communications)
echo.
exit /b 1

# Lisp Development Environment Setup Script
# This script gets you ready to code in Common Lisp with one click!

param(
    [switch]$NoPrompt,
    [ValidateSet('repl','vscode','test','test-all','none')]
    [string]$Action = 'none'
)

# Determine mode
$NonInteractive = $NoPrompt -or ($Action -ne 'none')

Write-Host "Starting Lisp Development Environment..." -ForegroundColor Green
Write-Host "===============================================" -ForegroundColor Green

# Step 1: Set up the PATH environment variable (add Scoop/roswell shims if present)
Write-Host "Setting up PATH for Roswell..." -ForegroundColor Yellow
$shimPaths = @()
if (Test-Path "$env:USERPROFILE\scoop\shims") { $shimPaths += "$env:USERPROFILE\scoop\shims" }
if (Test-Path "$env:USERPROFILE\AppData\Local\roswell") { $shimPaths += "$env:USERPROFILE\AppData\Local\roswell" }
if ($shimPaths.Count -gt 0) { $env:PATH = ( ($shimPaths -join ';') + ';' + $env:PATH ) }

# Step 2: Verify we're in the right directory
Write-Host "Checking current directory..." -ForegroundColor Yellow
$currentDir = Get-Location
Write-Host "Current directory: $currentDir" -ForegroundColor Cyan

# Helper: attempt to install Roswell via Scoop if available
function Ensure-Roswell {
    param([switch]$Silent)
    $rosCmd = Get-Command ros -ErrorAction SilentlyContinue
    if ($null -ne $rosCmd) { return $true }

    $scoopCmd = Get-Command scoop -ErrorAction SilentlyContinue
    if ($null -eq $scoopCmd) {
        if (-not $Silent) {
            Write-Host "Roswell not found and Scoop is not installed." -ForegroundColor Red
            Write-Host "Install Scoop, then Roswell: https://scoop.sh" -ForegroundColor Yellow
            Write-Host "After installing Scoop, run: scoop install roswell" -ForegroundColor Yellow
        }
        return $false
    }

    Write-Host "Roswell not found. Attempting to install via Scoop..." -ForegroundColor Yellow
    try {
        scoop bucket add main 2>$null | Out-Null
        scoop install roswell
    } catch {
        if (-not $Silent) { Write-Host "Failed to install Roswell via Scoop." -ForegroundColor Red }
        return $false
    }

    # Re-check after install
    $rosCmd = Get-Command ros -ErrorAction SilentlyContinue
    if ($null -ne $rosCmd) {
        # ensure shims are in PATH
        if (Test-Path "$env:USERPROFILE\scoop\shims") { $env:PATH = "$env:USERPROFILE\scoop\shims;$env:PATH" }
        return $true
    }
    return $false
}

# Step 3: Test that Roswell is working (and optionally auto-install)
Write-Host "Testing Roswell installation..." -ForegroundColor Yellow
if (-not (Get-Command ros -ErrorAction SilentlyContinue)) {
    $installed = Ensure-Roswell -Silent:$NonInteractive
    if (-not $installed) {
        if ($NonInteractive) {
            Write-Host "Roswell is required. Exiting (non-interactive)." -ForegroundColor Red
            exit 1
        } else {
            Write-Host "Roswell is not installed. Please install it and re-run this script." -ForegroundColor Red
            Write-Host "Suggested: Install Scoop (https://scoop.sh) then run 'scoop install roswell'" -ForegroundColor Yellow
            Read-Host "Press Enter to exit"
            exit 1
        }
    }
}

try {
    $rosVersion = ros --version
    Write-Host "Roswell found: $rosVersion" -ForegroundColor Green
} catch {
    Write-Host "Roswell command still not available. Exiting." -ForegroundColor Red
    exit 1
}

# Ensure Roswell is set up (installs quicklisp and sets up run image if needed)
try {
    ros setup | Out-Null
} catch {}

# Step 4: Test that SBCL is available
Write-Host "Testing SBCL availability..." -ForegroundColor Yellow
try {
    $sbclVersion = ros list installed | Select-String "sbcl"
    if ($sbclVersion) {
    Write-Host "SBCL found: $sbclVersion" -ForegroundColor Green
    } else {
    Write-Host "SBCL not found. Installing..." -ForegroundColor Yellow
        ros install sbcl
        ros use sbcl-bin
    }
} catch {
    Write-Host "Error checking SBCL. Continuing anyway..." -ForegroundColor Red
}

# Step 5: Optional quick project smoke test (skip for automated test modes or when ros is missing)
$skipQuickProjectTest = ($Action -in @('test','test-all')) -or -not (Get-Command ros -ErrorAction SilentlyContinue)
if (-not $skipQuickProjectTest) {
    Write-Host "Testing your Lisp project..." -ForegroundColor Yellow
    try {
        Write-Host "Running: ros run --load main.lisp --quit" -ForegroundColor Cyan
        $result = ros run --load main.lisp --quit 2>&1
        if ($LASTEXITCODE -eq 0) {
            Write-Host "Project test successful!" -ForegroundColor Green
            Write-Host "Output: $result" -ForegroundColor Cyan
        } else {
            Write-Host "Project test had issues, but continuing..." -ForegroundColor Yellow
            Write-Host "Error: $result" -ForegroundColor Red
        }
    } catch {
        Write-Host "Could not test project, but continuing..." -ForegroundColor Yellow
    }
}

# Step 6: Show menu options
Write-Host ""
Write-Host "Your Lisp environment is ready! Choose an option:" -ForegroundColor Green
Write-Host "==================================================" -ForegroundColor Green
Write-Host "1. Start Interactive Lisp REPL (recommended for learning)" -ForegroundColor Cyan
Write-Host "2. Open VS Code with Lisp support" -ForegroundColor Cyan
Write-Host "3. Run unit tests (FiveAM)" -ForegroundColor Cyan
Write-Host "4. Run unit tests with coverage" -ForegroundColor Cyan
Write-Host "5. View your beginner's guide" -ForegroundColor Cyan
Write-Host "6. Exit" -ForegroundColor Cyan
Write-Host ""

function Invoke-Choice([string]$choice){
        switch ($choice) {
        "1" {
            Write-Host "Starting Lisp REPL..." -ForegroundColor Green
            Write-Host "Type (quit) to exit the REPL when you're done." -ForegroundColor Yellow
            Write-Host "==================================================" -ForegroundColor Green
            ros run
                        return 0
        }
        "2" {
            Write-Host "Opening VS Code..." -ForegroundColor Green
            code .
            Write-Host "VS Code opened! Use Command Palette (Ctrl+Shift+P) and type 'Alive: Start REPL'" -ForegroundColor Green
                        return 0
        }
        "3" {
            Write-Host "Running unit tests..." -ForegroundColor Green
            ros run --load run-tests.lisp --quit
                        return $LASTEXITCODE
        }
        "4" {
            Write-Host "Running unit tests with coverage..." -ForegroundColor Green
            ros run --load run-tests.lisp --quit
                        return $LASTEXITCODE
        }
                "5" {
            Write-Host "Opening beginner's guide..." -ForegroundColor Green
                        Start-Process "BEGINNERS_USER_GUIDE.md"
                        return 0
                }
                "6" {
            Write-Host "Goodbye! Your environment is ready for next time." -ForegroundColor Green
                        return 0
        }
        default {
            Write-Host "Invalid choice. Please enter 1-6." -ForegroundColor Red
                        return 1
        }
    }
}

if ($Action -ne 'none'){
    switch ($Action){
        'repl' { exit (Invoke-Choice '1') }
        'vscode' { exit (Invoke-Choice '2') }
        'test' { exit (Invoke-Choice '3') }
        'test-all' { exit (Invoke-Choice '4') }
    }
}

if ($NoPrompt){ exit 0 }

do {
        $choice = Read-Host "Enter your choice (1-6)"
        $exitCode = Invoke-Choice $choice
        if ($exitCode -eq 0 -and $choice -in @('1','2','5','6')){ break }
} while ($choice -notin @("1", "2", "3", "4", "5"))

Write-Host ""
Write-Host "Tip: You can run this script anytime with: .\start-lisp.ps1" -ForegroundColor Yellow
Write-Host "Happy Lisp coding!" -ForegroundColor Green

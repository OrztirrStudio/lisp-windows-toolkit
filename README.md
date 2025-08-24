# Lisp Windows Toolkit

[![CI (Windows)](https://github.com/OrztirrStudio/lisp-windows-toolkit/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/OrztirrStudio/lisp-windows-toolkit/actions/workflows/ci.yml)
![LLM Assisted](https://img.shields.io/badge/LLM%20assisted-yes-8A2BE2)
[![Latest tag](https://img.shields.io/github/v/tag/OrztirrStudio/lisp-windows-toolkit?label=tag&sort=semver)](https://github.com/OrztirrStudio/lisp-windows-toolkit/tags)

Get coding in Common Lisp instantly with this automated Windows setup. Includes Roswell, SBCL, and Quicklisp.

One-click Common Lisp development environment for Windows.

Created by Nicholas Cole Akers.

## Quick start

Use the one-click setup scripts:

### Option 1: Double-click (easiest)

- Double-click `START-LISP.bat` in your project folder
- Press any key when prompted
- Choose from the menu that appears

### Option 2: PowerShell script

- Right-click `start-lisp.ps1` and "Run with PowerShell"
- Choose from the menu that appears

If Roswell isn't installed yet, the script will try to install it via Scoop (if available) or tell you how to install it. See QUICK-START.md for details.

## Usage

### Quick test (after running the setup script)

```powershell
ros run --load test.lisp --quit
```

If `ros` isn't on PATH yet, you can run the tests via the script:

```powershell
./start-lisp.ps1 -NoPrompt -Action test
```

### Advanced usage (original method)

```powershell
ros run -e "(ql:quickload :lisp-windows-toolkit) (asdf:load-system :lisp-windows-toolkit) (lisp-windows-toolkit:main)" --quit
```

### Run tests

From PowerShell:

```powershell
ros run -e "(ql:quickload :lisp-windows-toolkit/tests) (asdf:test-system :lisp-windows-toolkit/tests)" --quit
```

Or via the one-click script (non-interactive):

```powershell
./start-lisp.ps1 -NoPrompt -Action test
```

### Coverage report

On SBCL, coverage is collected with sb-cover automatically and saved to `coverage/`:

```powershell
./start-lisp.ps1 -NoPrompt -Action test-all
```

Artifacts include a `coverage.txt` report file.

To view the coverage artifact locally:

```powershell
Get-Content .\coverage\coverage.txt
```

In CI, download the “coverage” artifact from the workflow run (Actions tab).

## CI

This repo includes a GitHub Actions workflow (`.github/workflows/ci.yml`) that runs the test suite on Windows and uploads the `coverage/` artifact. Enable Actions on your fork to use it.

## Structure

- `lisp-windows-toolkit.asd`: ASDF system definition file
- `main.lisp`: Main source file containing the application logic
- `start-lisp.ps1`: One-click PowerShell setup script
- `START-LISP.bat`: Double-click batch file launcher
- `QUICK-START.md`: Complete setup instructions
- `BEGINNERS_USER_GUIDE.md`: Your personalized learning guide

## Development tools

### One-click setup scripts

- `start-lisp.ps1`: Full-featured PowerShell script with menu options
- `START-LISP.bat`: Simple double-click launcher
- `QUICK-START.md`: Complete setup and usage instructions

### What the scripts do

- Automatically set up your PATH for Roswell
- Test that everything is working
- Give you a menu of development options
- No more manual command typing!

## Tests

This project uses FiveAM for tests.

- Run tests from the command line:

```powershell
ros run -e "(ql:quickload :lisp-windows-toolkit/tests) (asdf:test-system :lisp-windows-toolkit/tests)" --quit
```

- Or inside a REPL:

```lisp
(ql:quickload :lisp-windows-toolkit/tests)
(asdf:test-system :lisp-windows-toolkit/tests)
```

## Run locally (Windows PowerShell)

```powershell
# From your project root
Set-Location "c:\Dev Projects\LISP\my-lisp-project"

# Optional: prepare PATH for this session
$env:PATH = "$env:USERPROFILE\scoop\shims;$env:USERPROFILE\AppData\Local\roswell;$env:PATH"

# Smoke test (should print the greeting)
ros run --load main.lisp --quit

# Run unit tests (writes coverage/coverage.txt on SBCL)
ros run --load run-tests.lisp --quit

# View coverage summary (if present)
Get-Content .\coverage\coverage.txt
```

Or use the one‑click script:

```powershell
.\start-lisp.ps1 -NoPrompt -Action test
```

## Troubleshooting (Windows)

- Roswell not found: ensure Scoop shims are in PATH for the session: `$env:PATH = "$env:USERPROFILE\scoop\shims;$env:USERPROFILE\AppData\Local\roswell;$env:PATH"`
- Execution policy: you can run `Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser`

See `QUICK-START.md` for more troubleshooting tips.

## Documentation

- **`QUICK-START.md`**: How to use the one-click setup scripts
- **`BEGINNERS_USER_GUIDE.md`**: Your complete learning guide
- **`SETUP_PROGRESS_REPORT.md`**: Technical setup details

## LLM assistance

This project includes content produced with LLM assistance to accelerate setup, documentation, and tests.

## Contributors

### Creator and Maintainer

- Nicholas Cole Akers - Project creator, initial development, and documentation

### Contributing

We welcome contributions. See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**Want to help?** Fork the repository and submit a pull request, this language needs you!

---

Made with LLM assistance.

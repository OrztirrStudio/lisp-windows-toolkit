# One-click Lisp development setup

## Quick start

### Option 1: Use the batch file

1. **Double-click `START-LISP.bat`** in your project folder
2. **Press any key** when prompted
3. **Choose your option** from the menu that appears

### Option 2: Run the PowerShell script

1. **Right-click `start-lisp.ps1`**
2. **Select "Run with PowerShell"**
3. **Choose your option** from the menu

## What the script does

- Sets up your PATH for Roswell
- Tests Roswell installation
- Verifies SBCL is available
- Runs basic project checks
- Shows a menu of options

## Menu options

### 1. Start interactive Lisp REPL

- Opens a live Lisp programming environment
- Type code and see results immediately
- Perfect for learning and experimenting
- Type `(quit)` to exit

### 2. Open VS Code with Lisp support

- Opens VS Code in your project folder
- Use Command Palette (Ctrl+Shift+P)
- Type "Alive: Start REPL" for VS Code Lisp support

### 3. Run unit tests (FiveAM)

- Loads and runs the FiveAM test suite
- Reports pass/fail status
- Good for checking after making changes

### 4. Run unit tests with coverage

- Runs tests and emits `coverage/coverage.txt`
- Works best on SBCL (uses sb-cover)

### 5. View the beginner's guide

- Opens your `BEGINNERS_USER_GUIDE.md`
- Full of examples and explanations
- Perfect for learning Lisp concepts

### 6. Exit

- Closes the script
- Your environment is still set up for the session

## Troubleshooting

### If you get a security error

- Right-click the PowerShell script
- Select "Properties"
- Check "Unblock" if available
- Or run: `Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser`

### If Roswell isn't found

- Make sure you've installed it with: `scoop install roswell`
- Check that Scoop is in your PATH

### If the script doesn't work

- Open PowerShell manually
- Navigate to your project: `cd "C:\Dev Projects\LISP\my-lisp-project"`
- Run: `.\start-lisp.ps1`

### Expected outputs

- Smoke test: `ros run --load main.lisp --quit` prints `Hello from Lisp Windows Toolkit!`
- Unit tests: exit code 0 with "passed" status; coverage at `coverage/coverage.txt` when using SBCL

## Tips

- **Run this script every time** you start coding
- **Keep the script open** while you're working
- **Use option 1** (REPL) for learning and experimenting
- **Use option 2** (VS Code) for serious development
- **Use option 3** to test changes quickly

## You're ready

With these scripts, you can start coding in Lisp without manual setup.

# Common Lisp beginner's user guide

## Day 4 programmer's reference

Welcome to Common Lisp. This guide is designed for early learners. It explains commands, concepts, and debugging techniques step-by-step.

---

## Quick start - daily workflow

### One-click setup (recommended)

Use the provided scripts to start quickly.

#### Option 1: Double-click (easiest)

1. **Double-click `START-LISP.bat`** in your project folder
2. **Press any key** when prompted
3. **Choose option 1** to start the Lisp REPL
4. **Type `(quit)`** when you're done

#### Option 2: PowerShell script

1. **Right-click `start-lisp.ps1`**
2. **Select "Run with PowerShell"**
3. **Choose option 1** to start the Lisp REPL

### What the scripts do

- Set up PATH for Roswell
- Verify basic functionality
- Provide a menu of options
- Reduce manual commands

### Manual setup

```powershell
# 1. Open PowerShell (Windows key + X, then select "Windows PowerShell")
# 2. Navigate to your project
cd "C:\Dev Projects\LISP\my-lisp-project"

# 3. Set up the PATH (you'll need this every session)
$env:PATH = "$env:USERPROFILE\scoop\shims;" + $env:PATH

# 4. Test that everything works
ros run --load main.lisp --quit
```
**Expected Output:** `Hello, LISP!`

If you see this, you're ready to code.

---

## Understanding Common Lisp basics

### What is Lisp?

- **LIS**t **P**rocessing language
- Everything is either an **atom** (like a number or word) or a **list** (things in parentheses)
- Code and data look the same - this is powerful!

### Basic syntax rules

```lisp
;; This is a comment (anything after ; or ;;)

;; Numbers
42          ; integer
3.14        ; decimal

;; Strings
"Hello"     ; text in quotes

;; Symbols (like variable names)
my-variable ; words with hyphens instead of underscores

;; Lists (the heart of Lisp)
(+ 1 2 3)   ; adds numbers: 1 + 2 + 3 = 6
(list 1 2 3) ; creates a list containing 1, 2, 3
```

### The most important rule: prefix notation
In most languages: `1 + 2`  
In Lisp: `(+ 1 2)`

The operator (like +) comes **first**, then the things it operates on.

---

## Your first Lisp programs

### 1. Basic math

```lisp
;; Addition
(+ 5 3)        ; Result: 8
(+ 1 2 3 4)    ; Result: 10 (adds all numbers)

;; Other operations
(- 10 3)       ; Subtraction: 7
(* 4 5)        ; Multiplication: 20
(/ 12 3)       ; Division: 4

;; Nested operations (like math with parentheses)
(+ (* 2 3) 4)  ; (2 * 3) + 4 = 10
```

### 2. Working with text

```lisp
;; Printing text
(format t "Hello, World!~%")  ; ~% means "new line"

;; Combining text
(format t "My name is ~a and I'm ~a years old~%" "Alex" 25)
```

### 3. Variables

```lisp
;; Create a variable
(defparameter *my-age* 25)  ; *asterisks* (called earmuffs) are a community convention for global variables.

;; Use the variable
(format t "I am ~a years old~%" *my-age*)
;; Local variables (only exist inside the 'let')
(let ((name "Alice")
      (age 30))
  (format t "~a is ~a years old~%" name age))
```

### 4. Functions

```lisp
;; Define a function
(defun greet (name)
  "This function greets someone"
  (format t "Hello, ~a!~%" name))

;; Function with multiple parameters
(defun add-numbers (a b)
  "Adds two numbers together"
  (+ a b))

;; Use them
(greet "Alex")
(add-numbers 5 7)  ; Result: 12
```

---


### Setup script menu options

When you run the setup script, you'll see this menu:

```text
Your Lisp environment is ready. Choose an option:
1. Start interactive Lisp REPL (recommended for learning)
2. Open VS Code with Lisp support
3. Test your project again
4. View the beginner's guide
5. Exit
```

**For beginners, choose option 1** - it starts the interactive Lisp environment!

### REPL commands (interactive programming)

```lisp
;; In VS Code, open Command Palette (Ctrl+Shift+P)
;; Type "Alive: Start REPL" to start interactive mode

;; Then you can type and test code immediately:
(+ 2 3)              ; Press Enter, see: 5
(format t "Hi!~%")   ; Press Enter, see: Hi!
```

### File operations

```lisp
;; Load a file
(load "my-file.lisp")

;; Load your project system
(asdf:load-system :lisp-windows-toolkit)

;; Run your main function
(lisp-windows-toolkit:main)
```

### Getting help

```lisp
;; Get help about a function
(describe 'format)   ; Shows information about the format function
(apropos "format")   ; Shows all functions with "format" in the name

;; See what functions are available in a package
(apropos "" :lisp-windows-toolkit)
```

---

## Debugging guide

### Common Error Messages and Solutions

#### 1. "The variable X is unbound"

**What it means:** You're trying to use a variable that doesn't exist.

```lisp
;; This will error:
(format t "~a" my-undefined-variable)

;; Fix: Define the variable first:
(defparameter my-variable "Hello")
(format t "~a" my-variable)
```

#### 2. "Undefined function"

**What it means:** You're calling a function that doesn't exist.

```lisp
;; This will error:
(my-nonexistent-function 42)

;; Fix: Make sure the function is defined or loaded:
(defun my-function (x) x)
(my-function 42)
```

#### 3. "illegal function call"

**What it means:** You have extra or missing parentheses.

```lisp
;; These will error:
((+ 1 2))     ; Extra parentheses
+ 1 2         ; Missing parentheses

;; Correct:
(+ 1 2)
```

#### 4. "System not found"

**What it means:** ASDF can't find your project.

```lisp
;; Fix: Add your project to the registry:
(push #P"./" asdf:*central-registry*)
(asdf:load-system :my-lisp-project)
```

### Debugging Techniques

#### 1. Use `format` to Print Values

```lisp
;; Add debug prints to see what's happening
(defun my-function (x)
  (format t "Debug: x is ~a~%" x)  ; Debug print
  (* x 2))
```

#### 2. Test Small Pieces

```lisp
;; Instead of writing a big function, test each part:
;; Step 1: Test the calculation
(* 5 2)

;; Step 2: Test the format
(format t "Result: ~a~%" 10)

;; Step 3: Combine them
(defun calculate-and-show (x)
  (let ((result (* x 2)))
    (format t "Result: ~a~%" result)))
```

#### 3. Use the REPL for Everything

- Type small expressions to test them
- Try functions with different inputs
- Check if variables have the right values

---

## 📚 Your Learning Path - Week by Week

### This Week (Days 4-7): Master the Basics

```lisp
;; Practice these every day:
;; 1. Basic math
(+ 1 2 3)
(* 4 5)

;; 2. Text output
(format t "Practice every day!~%")

;; 3. Simple functions
(defun double (x) (* x 2))
(double 5)
```

### Week 2: Lists and Control Flow

```lisp
;; Lists
(list 1 2 3 4)
(first (list 1 2 3))    ; Gets: 1
(rest (list 1 2 3))     ; Gets: (2 3)

;; Conditions
(if (> 5 3)
    "5 is bigger"
    "5 is not bigger")

;; Loops
(dotimes (i 5)
  (format t "Count: ~a~%" i))
```

### Week 3: More Advanced Concepts

```lisp
;; Recursion (functions that call themselves)
(defun countdown (n)
  (if (zerop n) ; zerop is a more specific and often clearer way to check for zero
      (format t "Blast off!~%")
      (progn
        (format t "~a~%" n)
        (countdown (1- n))))) ; 1- is a shorthand for (- n 1)
```

---

## 🛠️ Practical Exercises for Day 4-10

### Exercise 1: Calculator

```lisp
;; Create functions for basic math
(defun add (a b) (+ a b))
(defun subtract (a b) (- a b))
(defun multiply (a b) (* a b))
(defun divide (a b) (/ a b))

;; Test them:
(add 5 3)        ; Should give 8
(multiply 4 6)   ; Should give 24
```

### Exercise 2: Personal Information

```lisp
;; Create variables about yourself
(defparameter *my-name* "Your Name Here")
(defparameter *my-age* 25)
(defparameter *my-hobby* "Programming")

;; Create a function to introduce yourself
(defun introduce-myself ()
  (format t "Hi! I'm ~a, I'm ~a years old, and I love ~a!~%"
          *my-name* *my-age* *my-hobby*))

;; Call it:
(introduce-myself)
```

### Exercise 3: Number games

```lisp
;; Function to check if a number is even
(defun even? (n)
  (= (mod n 2) 0))  ; mod gives remainder of division

;; Test it:
(even? 4)   ; Should be T (true)
(even? 5)   ; Should be NIL (false)

;; Function to find the larger of two numbers
(defun larger (a b)
  (max a b)) ; It's good practice to use built-in functions when they exist!

;; Test it:
(larger 10 5)   ; Should give 10
```

---

## 🆘 When You Get Stuck

### 1. Read the error message carefully

- Look for the specific error type
- Find the line number if given
- Check for simple typos first

### 2. Use the REPL to test

```lisp
;; If this doesn't work:
(my-function 42)

;; Try this first:
(describe 'my-function)  ; Does the function exist?
```

### 3. Start simple and build up

```lisp
;; Instead of:
(defun complex-function (x y z)
  (format t "Result: ~a~%" (+ (* x y) z)))

;; Try:
(defun test-multiply (x y) (* x y))
(test-multiply 3 4)
;; Then:
(defun test-add (x y z) (+ x z))
;; Then combine them
```

### 4. Common Commands for Your Daily Work

#### Option A: One-click setup (recommended)

```powershell
# Just double-click START-LISP.bat or run start-lisp.ps1
# Choose option 1 to start the Lisp REPL
```

#### Option B: Manual setup (commands)

```powershell
# In PowerShell (run these when you start coding):
cd "C:\Dev Projects\LISP\my-lisp-project"
$env:PATH = "$env:USERPROFILE\scoop\shims;" + $env:PATH
code .
```

```lisp
;; In the REPL (for interactive testing):
(load "main.lisp")                    ; Load your main file
(my-lisp-project:main)               ; Run your main function
(describe 'function-name)             ; Get help on a function
(apropos "keyword")                   ; Find functions with keyword
```

---

## 📖 Recommended Daily Practice

### Every day (15-30 minutes)

1. **Start your environment** (double-click `START-LISP.bat` or run `start-lisp.ps1`)
2. **Choose option 1** to start the Lisp REPL
3. **Try 2-3 new expressions** in the REPL
4. **Modify your main.lisp** file with something new
5. **Test it** with `ros run --load main.lisp --quit`

### Example daily challenges

- **Day 4:** Make your program print your name and age
- **Day 5:** Add a function that calculates your age in months
- **Day 6:** Create a function that tells you how many days until your next birthday
- **Day 7:** Make a simple guessing game (pick a number 1-10)

---

## 🎯 Success Tips for New Programmers

1. **Don't worry about perfection** - Focus on making it work first
2. **Test everything small** - Try each piece separately
3. **Use lots of debug prints** - `(format t "Debug: ~a~%" variable)`
4. **Save your work often** - Ctrl+S after every small change
5. **Don't be afraid of errors** - They're how you learn!

---

## 📞 Quick Reference Card

### Essential commands

```lisp
;; Math
(+ 1 2)         ; Add
(- 5 3)         ; Subtract
(* 4 3)         ; Multiply
(/ 8 2)         ; Divide

;; Text
(format t "Hello~%")               ; Print text
(format t "Value: ~a~%" variable)  ; Print variable

;; Variables
(defparameter *name* "value")      ; Global variable
(let ((x 5)) ...)                  ; Local variable

;; Functions
(defun name (param) body)          ; Define function
(function-name argument)           ; Call function

;; Help
(describe 'function-name)          ; Get help
(apropos "search-term")            ; Find functions
```

### File commands

```powershell
# Quick setup (recommended)
# Double-click START-LISP.bat or run start-lisp.ps1

# Manual setup
cd "C:\Dev Projects\LISP\my-lisp-project"
$env:PATH = "$env:USERPROFILE\scoop\shims;" + $env:PATH

# Test your program
ros run --load main.lisp --quit

# Open VS Code
code .
```

---

**Remember:** Programming is like learning a new language. Day 4 is still very early! Be patient with yourself, practice daily, and celebrate small victories. Every error message is a learning opportunity! 🌟

**Good luck on your Lisp journey!** 🚀

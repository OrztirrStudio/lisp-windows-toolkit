# Contributing to Lisp Windows Toolkit

Thank you for your interest in contributing to Lisp Windows Toolkit! This document provides guidelines and information for contributors.

## Quick start

1. **Fork the repository**
2. **Create a feature branch** (`git checkout -b feature/amazing-feature`)
3. **Make your changes**
4. **Test your changes** thoroughly
5. **Commit your changes** (`git commit -m 'Add amazing feature'`)
6. **Push to the branch** (`git push origin feature/amazing-feature`)
7. **Open a Pull Request**

## What we're looking for

### High priority
- **Bug fixes** for Windows compatibility issues
- **Improvements** to the setup scripts
- **Documentation** updates and clarifications
- **Testing** on different Windows versions

### Medium priority
- **New features** for the toolkit
- **Performance improvements**
- **Code quality** enhancements
- **Cross-platform** considerations

### Low priority
- **Cosmetic changes** (unless they improve usability)
- **Major refactoring** (discuss first)

## Development setup

### Prerequisites
- Windows 10/11
- PowerShell 5.1+
- Git
- Common Lisp knowledge (helpful but not required)

### Local development
1. **Clone your fork:**
   ```bash
   git clone https://github.com/OrztirrStudio/lisp-windows-toolkit.git
   cd lisp-windows-toolkit
   ```

2. **Test the current setup:**
   ```bash
   .\start-lisp.ps1
   ```

3. **Make your changes** and test them

## Code style guidelines

### PowerShell scripts
- Use **PascalCase** for function names
- Use **camelCase** for variables
- Add **comments** explaining complex logic
- Use **consistent indentation** (2 spaces)
- **Handle errors** gracefully with try-catch blocks

### Lisp code
- Follow **Common Lisp conventions**
- Use **descriptive function names**
- Add **documentation strings** for functions
- **Test your code** before submitting

### Documentation
- Use **clear, simple language**
- Include **examples** where helpful
- **Test instructions** before documenting
- Use **consistent formatting**

## Testing guidelines

### Before submitting
1. **Test on Windows 10** (minimum supported version)
2. **Test on Windows 11** if possible
3. **Test with different PowerShell versions**
4. **Verify scripts work** from different directories
5. **Check error handling** works correctly

### **Testing Checklist:**
- [ ] Setup script runs without errors
- [ ] Lisp environment starts correctly
- [ ] All menu options work
- [ ] Error messages are helpful
- [ ] Documentation is accurate

## Pull request guidelines

### Title format
- **Feature:** `Add new feature: description`
- **Bug fix:** `Fix issue: description`
- **Documentation:** `Update docs: description`
- **Refactor:** `Refactor: description`

### Description template
```markdown
## What does this PR do?
Brief description of the changes

## Why is this change needed?
Explanation of the problem or improvement

## How was this tested?
Description of testing steps

## Screenshots (if applicable)
Add screenshots for UI changes

## Checklist
- [ ] Code follows style guidelines
- [ ] Tests pass locally
- [ ] Documentation updated
- [ ] No breaking changes
```

## Bug reports

### Bug report template
```markdown
## Bug Description
Clear description of what's not working

## Steps to Reproduce
1. Step 1
2. Step 2
3. Step 3

## Expected Behavior
What should happen

## Actual Behavior
What actually happens

## Environment
- Windows Version: [e.g., Windows 10 21H2]
- PowerShell Version: [e.g., 5.1.19041.1]
- Lisp Implementation: [e.g., SBCL 2.5.7]

## Additional Information
Any other relevant details
```

## Feature requests

### Feature request template
```markdown
## Feature Description
Clear description of the new feature

## Use Case
Why this feature would be useful

## Proposed Implementation
How you think it should work

## Alternatives Considered
Other approaches you've thought about

## Additional Context
Any other relevant information
```

## Code of conduct

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## Getting help

- **GitHub Issues:** For bugs and feature requests
- **GitHub Discussions:** For questions and general discussion
- **Pull Requests:** For code contributions

## Recognition

Contributors will be recognized in:
- **README.md** contributors section
- **Release notes** for significant contributions
- **GitHub contributors** page

Thank you for contributing to making Lisp development on Windows easier for everyone.

--
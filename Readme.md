# Flymake Python Backends

An Emacs package providing Flymake backends for Python using `mypy`, `flake8`, and `pylint`.

## Introduction

`flymake-python-backends` is an Emacs package that integrates Python linters and type checkers with Flymake, providing on-the-fly syntax checking and linting. It supports popular tools like `mypy`, `flake8`, and `pylint`, helping you catch errors and improve code quality as you write.

## Features

- **Mypy Backend**: Static type checking with `mypy`.
- **Flake8 Backend**: Code style enforcement and linting with `flake8`.
- **Pylint Backend**: Comprehensive code analysis with `pylint`.
- **Seamless Integration**: Easily integrates with Flymake's diagnostic system.
- **Customizable**: Configure commands and options for each backend.

## Installation

### Dependencies

- **Emacs**: Version 26.1 or higher.
- **Flymake**: Built-in since Emacs 26.1.
- **[f.el](https://github.com/rejeep/f.el)**: File management utility library for Emacs.

Ensure that `mypy`, `flake8`, and/or `pylint` are installed and available in your system's `PATH`.

### Using `use-package`

If you use `use-package` with `straight.el` or `package.el`, you can install `flymake-python-backends` as follows:

```emacs-lisp
(use-package f
  :ensure t)

(use-package flymake-python-backends
  :load-path "/path/to/flymake-python-backends"
  :config
  ;; Enable desired backends
  (add-hook 'python-mode-hook #'flymake-python-backends/setup-mypy-flymake-backend)
  (add-hook 'python-mode-hook #'flymake-python-backends/setup-flake8-flymake-backend)
  (add-hook 'python-mode-hook #'flymake-python-backends/setup-pylint-flymake-backend))
```

## Usage

### Enabling Backends

Add the setup functions for the backends you wish to use to your `python-mode-hook`:

```emacs-lisp
;; Enable Mypy backend
(add-hook 'python-mode-hook #'flymake-python-backends/setup-mypy-flymake-backend)

;; Enable Flake8 backend
(add-hook 'python-mode-hook #'flymake-python-backends/setup-flake8-flymake-backend)

;; Enable Pylint backend
(add-hook 'python-mode-hook #'flymake-python-backends/setup-pylint-flymake-backend)
```

You can enable one or multiple backends simultaneously.

### Customization

You can customize the commands and options used by each backend:

- **Mypy Command**:

  ```emacs-lisp
  (setq flymake-python/mypy-command '("mypy" "--show-column-numbers"))
  ```

- **Flake8 Command**:

  ```emacs-lisp
  (setq flymake-python/flake8-command '("flake8"))
  ```

- **Pylint Command**:

  ```emacs-lisp
  (setq flymake-python/pylint-command '("pylint"))
  ```

These variables can also be customized via Emacs' customization interface:

```emacs-lisp
M-x customize-group RET flymake-python-backends RET
```

## Requirements

- **Emacs**: Version 26.1 or higher.
- **Flymake**: Built-in package for on-the-fly syntax checking.
- **[f.el](https://github.com/rejeep/f.el)**: File management library.

Ensure the following Python tools are installed:

- [**mypy**](http://mypy-lang.org/)
- [**flake8**](http://flake8.pycqa.org/)
- [**pylint**](https://www.pylint.org/)

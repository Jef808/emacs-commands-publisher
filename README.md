# ECP - Emacs Context Publisher
Extract and publish interactive command events from Emacs to external
services for context tracking and analysis.



## Features
- Captures interactive commands with context (buffer, project,
  timestamp)
- Asynchronous HTTP publishing to configurable endpoints
- Session tracking with unique identifiers
- Configurable command exclusion list
- Simple enable/disable toggle

## Installation
### Doom Emacs
Add to your =packages.el=:

``` elisp
(package! ecp
  :recipe (:host github :repo "jef808/emacs-commands-publisher"))
```

Add to your =config.el=:

``` elisp
(use-package! ecp
  :commands (ecp-enable)
  :custom
  ecp-publish-url "http://localhost:8000/emacs-events/"
  ecp-publish-timeout 5
  ecp-excluded-commands '(self-insert-command
                          delete-backward-char
                          forward-char
                          backward-char
                          next-line
                          previous-line
                          mouse-set-point
                          handle-switch-frame
                          mwheel-scroll)
  :config
  (ecp-enable))
```

Run =doom sync= to install.

## Usage
``` elisp
;; Set publishing endpoint
(ecp-set-url "https://your-endpoint.com/events")

;; Enable publishing
(ecp-enable)

;; Check status
(ecp-status)

;; Disable publishing
(ecp-disable)
```

## Configuration
- =ecp-publish-url=: HTTP endpoint for event publishing
- =ecp-publish-timeout=: Request timeout in seconds
- =ecp-excluded-commands=: Commands to exclude from publishing

## Event Format
Events are published as JSON with timestamp, session ID, command name,
buffer info, and current project.

## Backend Server

I use [this very simple http server](https://github.com/Jef808/chrome-ext-export-history) as the backend to store commands in a sqlite db. Feel free to use it as is or to use it as a base for your own needs.

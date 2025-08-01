;;; ecp.el --- Publish interactive commands with context -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jean-François Arbour
;;
;; Author: Jean-François Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-François Arbour <jf.arbour@gmail.com>
;; Created: July 26, 2025
;; Modified: July 26, 2025
;; Version: 0.0.1
;; Keywords: convenience extensions hypermedia lisp local matching outlines processes tools
;; Homepage: https://github.com/jfa/ecp
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Publish interactive commands with context
;;
;;; Code:

(require 'json)
(require 'url)
(require 'project)

(defgroup ecp nil
  "Extract context events from Emacs."
  :group 'convenience
  :prefix "ecp-")

(defcustom ecp-publish-url nil
  "URL endpoint to publish interactive command events.
If nil, publishing is disabled."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "URL"))
  :group 'ecp)

(defcustom ecp-publish-timeout 5
  "Timeout in seconds for HTTP requests."
  :type 'integer
  :group 'ecp)

(defcustom ecp-enable-logging nil
  "Whether to log published events to a buffer.
When non-nil, events are logged to the *ECP Log* buffer."
  :type 'boolean
  :group 'ecp)

(defcustom ecp-log-buffer-name "*ECP Log*"
  "Name of the buffer used for logging events."
  :type 'string
  :group 'ecp)

(defcustom ecp-log-max-entries 1000
  "Maximum number of log entries to keep in the log buffer.
When exceeded, older entries are removed."
  :type 'integer
  :group 'ecp)

(defvar ecp--log-entry-count 0
  "Current number of entries in the log buffer.")

(defcustom ecp-excluded-commands
  '(self-insert-command
    delete-backward-char
    forward-char
    backward-char
    next-line
    previous-line
    mouse-set-point
    handle-switch-frame
    mwheel-scroll)
  "List of commands to exclude from publishing."
  :type '(repeat symbol)
  :group 'ecp)

(defvar ecp--enabled nil
  "Whether command publishing is enabled.")

(defvar ecp--session-id nil
  "Unique session identifier for this Emacs instance.")

(defvar ecp--last-context nil
  "Last recorded context state.")

(defvar ecp--last-command-time 0
  "Timestamp of the last executed command.")

(defvar ecp--command-burst-threshold 600
  "Seconds of inactivity to consider next command significant.")

(defun ecp--generate-session-id ()
  "Generate a unique session identifier."
  (format "%d-%d"
          (emacs-pid)
          (floor (float-time))))

(defun ecp--get-log-buffer ()
  "Get or create the ECP log buffer."
  (let ((buffer (get-buffer-create ecp-log-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'ecp-log-mode)
        (ecp-log-mode)))
    buffer))

(defun ecp--log-event (payload)
  "Log event PAYLOAD to the log buffer."
  (when ecp-enable-logging
    (let ((buffer (ecp--get-log-buffer))
          (timestamp (plist-get payload :timestamp))
          (command (plist-get payload :command))
          (context (plist-get payload :context)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "[%s] %s\n" timestamp command))
          (when context
            (insert (format "  Context: %s\n"
                            (mapconcat (lambda (kv)
                                         (format "%s=%s"
                                                 (substring (symbol-name (car kv)) 1)
                                                 (cadr kv)))
                                       (seq-partition context 2)
                                       " "))))
          (insert "\n")
          (setq ecp--log-entry-count (1+ ecp--log-entry-count))

          ;; Trim log if it exceeds maximum entries
          (when (> ecp--log-entry-count ecp-log-max-entries)
            (ecp--trim-log-buffer)))))))

(defun ecp--trim-log-buffer ()
  "Remove oldest entries from log buffer to stay within limits."
  (with-current-buffer (ecp--get-log-buffer)
    (let ((inhibit-read-only t)
          (entries-to-remove (/ ecp-log-max-entries 10)))
      (goto-char (point-min))
      (dotimes (_ entries-to-remove)
        (when (re-search-forward "^\\[.*?\\].*?\n\\(?:  Context:.*?\n\\)?\n" nil t)
          (delete-region (match-beginning 0) (match-end 0))
          (setq ecp--log-entry-count (1- ecp--log-entry-count)))))))

(define-derived-mode ecp-log-mode special-mode "ECP-Log"
  "Major mode for ECP event log buffer."
  (setq buffer-read-only t)
  (setq truncate-lines nil))

(defun ecp--get-current-context ()
  "Get current meaningful context."
  (list :buffer (buffer-name)
        :file_name (buffer-file-name)
        :major_mode major-mode
        :project (when-let ((proj (project-current)))
                   (project-name proj))))

(defun ecp--context-changed-p ()
  "Retur non-nil if context has meaningfully changed."
  (let ((current (ecp--get-current-context)))
    (prog1 (not (equal current ecp--last-context))
      (setq ecp--last-context current))))

(defun ecp--is-significant-activity-p ()
  "Return non-nil if the current command represents significant activity.
This is determined by checking if the time since the last command
exceeds `ecp--command-burst-threshold'."
  (let ((now (float-time)))
    (prog1 (> (- now ecp--last-command-time) ecp--command-burst-threshold)
      (setq ecp--last-command-time now))))

(defun ecp--should-publish-command-p (command)
  "Return non-nil if COMMAND should be published."
  (and ecp--enabled
       ecp-publish-url
       (not (memq command ecp-excluded-commands))
       (or (ecp--context-changed-p)
           (ecp--is-significant-activity-p))))

(defun ecp--create-event-payload (command)
  "Create event payload for COMMAND."
  (let ((base-payload (list :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t)
                            :host (system-name)
                            :session_id ecp--session-id
                            :command (symbol-name command)
                            :context ecp--last-context)))
    base-payload))

(defun ecp--publish-event-async (payload)
  "Publish event PAYLOAD asynchronously."
  (ecp--log-event payload)
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
    (url-retrieve ecp-publish-url
                  (lambda (status)
                    (when-let ((error (plist-get status :error)))
                      (message "ECP: Failed to publish event: %s" error)))
                  nil
                  'silent)))

(defun ecp--command-hook ()
  "Hook function to capture and publish interactive commands."
  (when (and (ecp--should-publish-command-p this-command)
             (not (or executing-kbd-macro noninteractive)))
    (let ((payload (ecp--create-event-payload this-command)))
      (ecp--publish-event-async payload))))

;;;###autoload
(defun ecp-enable ()
  "Enable interactive command publishing."
  (interactive)
  (unless ecp--session-id
    (setq ecp--session-id (ecp--generate-session-id)))
  (setq ecp--enabled t)
  (add-hook 'post-command-hook #'ecp--command-hook)
  (message "ECP: Command publishing enabled (session: %s)" ecp--session-id))

;;;###autoload
(defun ecp-disable ()
  "Disable interactive command publishing."
  (interactive)
  (setq ecp--enabled nil)
  (remove-hook 'post-command-hook #'ecp--command-hook)
  (message "ECP: Command publishing disabled"))

;;;###autoload
(defun ecp-toggle ()
  "Toggle interactive command publishing."
  (interactive)
  (if ecp--enabled
      (ecp-disable)
    (ecp-enable)))

;;;###autoload
(defun ecp-toggle-logging ()
  "Toggle event logging on/off."
  (interactive)
  (setq ecp-enable-logging (not ecp-enable-logging))
  (message "ECP: Logging %s" (if ecp-enable-logging "enabled" "disabled")))

;;;###autoload
(defun ecp-show-log ()
  "Display the ECP event log buffer."
  (interactive)
  (let ((buffer (ecp--get-log-buffer)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (goto-char (point-max)))))

;;;###autoload
(defun ecp-clear-log ()
  "Clear the ECP event log buffer."
  (interactive)
  (when (get-buffer ecp-log-buffer-name)
    (with-current-buffer ecp-log-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq ecp--log-entry-count 0))))
  (message "ECP: Log cleared"))

;;;###autoload
(defun ecp-status ()
  "Show current ECP status."
  (interactive)
  (message "ECP: %s (URL: %s, Session: %s, Logging: %s)"
           (if ecp--enabled "Enabled" "Disabled")
           (or ecp-publish-url "Not configured")
           (or ecp--session-id "None")
           (if ecp-enable-logging "On" "Off")))

;;;###autoload
(defun ecp-set-url (url)
  "Set the publishing URL to URL."
  (interactive "sPublish URL: ")
  (setq ecp-publish-url (if (string-empty-p url) nil url))
  (message "ECP: Publish URL set to %s" (or ecp-publish-url "disabled")))

(provide 'ecp)
;;; ecp.el ends here

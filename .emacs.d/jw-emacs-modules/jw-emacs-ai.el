(use-package gptel
  :ensure t
  :after auth-source
  :init
  ;; Ensure auth-source is configured to find ~/.authinfo or ~/.authinfo.gpg
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
  :config
  ;; Helper function to read file contents
  (defun gptel-read-file (file-path)
      "Read the contents of FILE-PATH into a string, trimming whitespace."
      (if (and (file-exists-p file-path) (file-readable-p file-path))
          (with-temp-buffer
          (insert-file-contents file-path)
          (string-trim (buffer-string)))
      (progn
          (message "Warning: File %s is not readable or does not exist" file-path)
          "You are a polymath who is a helpful assistant. Respond concisely and accurately.")))

  ;; Define directives with file paths
  (setq gptel-directives
          (list
          (cons 'default "You are a polymath who is a helpful assistant. Respond concisely and accurately.")
          (cons 'coding (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/coding.txt"))
          (cons 'writing (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/writing.txt"))
          (cons 'research (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/research.txt"))))

  ;; Set default directive
  (setq gptel-default-directive 'coding)

  
  ;; Refresh directives dynamically
  (defun gptel-refresh-directives ()
      "Refresh gptel-directives by re-reading files."
      (interactive)
      (setq gptel-directives
          (list
          (cons 'default "You are a polymath who is a helpful assistant. Respond concisely and accurately.")
          (cons 'coding (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/coding.txt"))
          (cons 'writing (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/writing.txt"))
          (cons 'research (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/research.txt"))))
      (message "Refreshed gptel-directives from files."))

  ;; Set OpenAI API key using gptel-api-key-from-auth-source
  (setq gptel-api-key (lambda ()
                        (auth-source-forget-all-cached)
                        (gptel-api-key-from-auth-source)))
  ;; Configure Google (Gemini) backend
  (gptel-make-gemini "Gemini" :stream t
                      :key (lambda ()
           (auth-source-forget-all-cached)
           (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")))
  ;; Configure Anthropic (Claude) backend
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda ()
           (auth-source-forget-all-cached)
           (gptel-api-key-from-auth-source "console.anthropic.com")))
  ;; Optional: Enable debugging for auth-source issues
  ;; (setq auth-source-debug t)
)

(defun gptel-save-response ()
"Save the entire gptel buffer to a file with a user-provided name."
(interactive)
(unless (bound-and-true-p gptel-mode)
    (user-error "This command must be run in a gptel-mode buffer"))
(let* ((response (buffer-string))
        (user-input (read-string "Enter a concise (2-5 words) filename description: ")))
    (if (string-empty-p response)
        (message "Error: Buffer is empty, cannot save file")
    (let* ((clean-name (if (and user-input (stringp user-input) (not (string-empty-p user-input)))
                            (string-trim (replace-regexp-in-string "[^a-zA-Z0-9-]" "" (replace-regexp-in-string "\\s+" "-" user-input)))
                        "fallback-name"))
            (timestamp (format-time-string "%Y%m%dT%H%M%S"))
            (base-dir "~/Otzar/llm-outputs/")
            (filename (concat (file-name-as-directory (expand-file-name base-dir)) timestamp "--" clean-name ".md")))
        (condition-case err
            (progn
            (make-directory base-dir t)
            (write-region (point-min) (point-max) filename nil 'silent)
            (message "Saved buffer to %s" filename))
        (error
        (message "Error saving file: %s" err)))))))

(provide 'jw-emacs-ai)

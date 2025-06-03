(use-package gptel
  :ensure t
  :after auth-source
  :init
  ;; Ensure auth-source is configured to find ~/.authinfo or ~/.authinfo.gpg
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
  :config
  ;; Set OpenAI API key using gptel-api-key-from-auth-source
  (setq gptel-api-key (lambda ()
                        (auth-source-forget-all-cached)
                        (gptel-api-key-from-auth-source)))
  ;; Configure Anthropic (Claude) backend
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda ()
           (auth-source-forget-all-cached)
           (gptel-api-key-from-auth-source "console.anthropic.com")))
  ;; Optional: Enable debugging for auth-source issues
  ;; (setq auth-source-debug t)
)

(provide 'jw-emacs-ai)

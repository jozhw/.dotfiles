(defun jw-dired-get-file-path ()
  "Get the full path of the file at point in Dired mode and display it in the minibuffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (message file)
    (kill-new file)))

(define-key dired-mode-map (kbd "C-c p") 'jw-dired-get-file-path)

(defun jw-copy-file (source-file destination-file)
  "Copy a file from SOURCE-FILE to DESTINATION-FILE."
  (interactive "FSource file: \nFDestination file: ")
  (if (file-exists-p source-file)
      (let ((dest-dir (file-name-directory destination-file)))
        (if (file-exists-p dest-dir)
            (if (not (file-exists-p destination-file))
                (progn
                  (copy-file source-file destination-file)
                  (message "File copied successfully."))
              (message "Destination file already exists. Choose another destination."))
          (message "Destination directory does not exist.")))
    (message "Source file does not exist.")))

(provide 'jw-emacs-copy)

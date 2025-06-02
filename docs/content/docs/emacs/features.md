---
weight: 1
bookFlatSection: true
title: "Features"
---

# Introduction

The features are implementations on top of the native emacs packages and not third-party packages.


# Sound Support

Usually this is a problem for macos and I found a snippet of code that enables sound support. The way to tell is by running `M-x play-sound-file` and navigating to the `.wav` file will ouput "This Emacs binary lacks sound support."

```emacs-lisp
  ;; on macos, fix "This Emacs binary lacks sound support" 
  ;; - https://github.com/leoliu/play-sound-osx/blob/master/play-sound.el
  ;; - update according to https://github.com/leoliu/play-sound-osx/issues/2#issuecomment-1088360638
  (when (eq system-type 'darwin)
    (unless (and (fboundp 'play-sound-internal)
                 (subrp (symbol-function 'play-sound-internal)))
      (defun play-sound-internal (sound)
        "Internal function for `play-sound' (which see)."
        (or (eq (car-safe sound) 'sound)
            (signal 'wrong-type-argument (list sound)))
      
        (cl-destructuring-bind (&key file data volume device)
            (cdr sound)
        
          (and (or data device)
               (error "DATA and DEVICE arg not supported"))
        
          (apply #'start-process "afplay" nil
                 "afplay" (append (and volume (list "-v" volume))
                                  (list (expand-file-name file data-directory))))))))

```

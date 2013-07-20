;;; sps-mode.el  --- Tools for interactive Parenscript development

;; Copyright (C) 2013 John Mastro

;; Author: John Mastro <john.b.mastro@gmail.com>
;; Version: 0.0.1
;; Keywords: languages, lisp, processes, tools
;; Package-Requires: ((slime "2013-05-26") (skewer-mode "1.5.0")
;;                    (s "1.6.1") (dash "1.0.3"))

;;; Commentary:

;; This is an Emacs minor mode and collection of commands for working with
;; Parenscript code in SLIME and sending it to the browser via Skewer. The goal
;; is to create an environment for hacking Parenscript which fits as naturally
;; as possible into the Lisp style of interactive development.

;; See https://github.com/johnmastro/sps-mode for additional information.

;; Note that this is very young code and there are certain to be serious
;; problems.

;;; Code:

(require 'dash)
(require 's)

;;;; Vars

(defvar sps-mode-map (make-sparse-keymap)
  "Keymap for sps-mode.
This keymap is initialized empty. You can optionally use
`sps-add-keys-with-prefix` to add bindings for all commands on
two-key sequences behind a prefix of your choice.")

(defvar sps-expansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "x") 'sps-send-expanded-code)
    map)
  "Keymap for `sps-expansion-mode` buffers.")

(defvar sps-scratch-mode-map (make-sparse-keymap)
  "Keymap for the *sps-scratch* buffer.")

(defvar sps-expansion-major-mode 'javascript-mode
  "The major mode to enable in expansion buffers.
Note that a serious bug seems to be triggered when using js2-mode
in expansion buffers. Avoid it for the time being unless you know
what you're doing."
  ;; It would be better to use js2-mode, especially since Skewer requires it
  ;; anyway (why make people load two different JavaScript modes)? But, I've
  ;; come across a bug somehow related to js2-mode and skewer - when an
  ;; expansion is loaded in a js2-mode buffer the code is unexpectedly
  ;; evaluated (even though we haven't called skewer-eval) and, worse, Emacs
  ;; occasionally freezes. Until I track this down I don't want to leave the
  ;; code laying around with js2-mode as the default in case anyone happens
  ;; across it and gives it a try.
  )

;;;; A few utilities

(defun sps-wrap-in-ps-form (string)
  "Return Parenscript STRING wrapped in a PS:PS form."
  (format "(ps:ps %s)" string))

(defun sps-swank-eval-expr (string)
  "Return a form appropriate for evaluating STRING via Swank."
  `(swank:eval-and-grab-output ,(sps-wrap-in-ps-form string)))

(defun sps-file-name-base (filename)
  "Return FILENAME's basename without it's extension."
  (file-name-sans-extension (file-name-nondirectory filename)))

;;;; Code expansion

(defun sps-expand (string)
  "Display the JavaScript generated from Parenscript STRING.

The resulting JavaScript is displayed in a temporary buffer. The
buffer's major mode is determined by the variable
`sps-expansion-major-mode` (`javascript-mode` by default).
`sps-expansion-mode` is enabled as an additional minor mode."
  (slime-eval-async (sps-swank-eval-expr string)
    #'(lambda (result)
        (let ((code (read (cadr result))))
          (slime-with-popup-buffer ("*Parenscript generated JavaScript*")
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert code)
            (funcall sps-expansion-major-mode)
            (sps-expansion-mode 1)
            (font-lock-fontify-buffer)
            (goto-char (point-min))
            (setq buffer-read-only t)
            (pop-to-buffer (current-buffer)))))
    (slime-current-package)))

;; (defun sps-compile-buffer-to-file (&optional append)
;;   (interactive "p")
;;   (when (and (buffer-modified-p)
;;              (y-or-n-p "Save buffer? "))
;;     (save-buffer))
;;   (let* ((this buffer-file-name)
;;          (dir (and this (file-name-directory this)))
;;          (initial (and this (concat (sps-file-name-base this) ".js")))
;;          (destination (read-file-name "Destination: " dir nil nil initial nil))
;;          (string (buffer-substring-no-properties (point-min) (point-max))))
;;     (slime-eval-async (sps-swank-eval-expr string)
;;       #'(lambda (result)
;;           (let ((code (read (cadr result))))
;;             (write-region code nil destination append)))
;;       (slime-current-package))))

(defun sps-expand-sexp ()
  "Display the expansion of the form at point."
  (interactive)
  (sps-expand (slime-sexp-at-point)))

(defun sps-expand-last-expression ()
  "Display the expansion of the expression preceding point."
  (interactive)
  (sps-expand (slime-last-expression)))

(defun sps-expand-defun ()
  "Display the expansion of the current toplevel form."
  (interactive)
  (sps-expand (slime-defun-at-point)))

(defun sps-expand-region (beg end)
  "Display the expansion of the currently active region."
  (interactive "r")
  (sps-expand (buffer-substring-no-properties beg end)))

(defun sps-expand-buffer ()
  "Display the expansion of the current buffer."
  (interactive)
  (sps-expand-region (point-min) (point-max)))

(defun sps-expand-dwim ()
  "Display the expansion of the active region or toplevel form.
If the region is active this is equivalent to invoking
`sps-expand-region`, otherwise it's equivalent to
`sps-expand-defun`."
  (interactive)
  (if (region-active-p)
      (sps-expand-region (region-beginning) (region-end))
    (sps-expand-defun)))

;;;; Code evaluation

(defun sps-eval (string)
  "Compile Parenscript STRING and evaluate it in the browser.

The code is first compiled to JavaScript in the CL image and then
sent to the browser via `skewer-eval`."
  (slime-eval-async (sps-swank-eval-expr string)
    #'(lambda (result)
        (let ((code (read (cadr result))))
          (skewer-eval code #'skewer-post-minibuffer)))
    (slime-current-package)))

(defun sps-eval-sexp ()
  "Evaluate the expression at point as Parenscript."
  (interactive)
  (sps-eval (slime-sexp-at-point)))

(defun sps-eval-last-expression ()
  "Evaluate the expression preceding point as Parenscript."
  (interactive)
  (sps-eval (slime-last-expression)))

(defun sps-eval-defun ()
  "Evaluate the current toplevel form as Parenscript."
  (interactive)
  (sps-eval (slime-defun-at-point)))

(defun sps-eval-region (beg end)
  "Evaluate the currently active region as Parenscript."
  (interactive "r")
  (sps-eval (buffer-substring-no-properties beg end)))

(defun sps-eval-buffer ()
  "Evaluate the current buffer as Parenscript."
  (interactive)
  (sps-eval-region (point-min) (point-max)))

(defun sps-eval-dwim ()
  "Evaluate the active region or toplevel form.
If the region is active this is equivalent to invoking
`sps-eval-region`, otherwise it's equivalent to
`sps-eval-defun`."
  (if (region-active-p)
      (sps-eval-region (region-beginning) (region-end))
    (sps-eval-defun)))

(defun sps-send-expanded-code ()
  "Send the expanded code to the browser.
For use from `sps-expansion-mode` buffers."
  (interactive)
  (skewer-eval
   (buffer-substring-no-properties (point-min) (point-max))
   #'skewer-post-minibuffer))

;;;; Keybindings

(defun sps-prefix-keys (prefix keys)
  "Prepend PREFIX to KEYS and read with `read-kbd-macro`."
  (read-kbd-macro (concat prefix " " keys)))

(defun sps-add-keys-with-prefix (p)
  "Add keybindings for `sps-mode` commands behind prefix P."
  (let ((map sps-mode-map))
    ;; Evaluation commands
    (define-key map (sps-prefix-keys p "e C-m") 'sps-eval-sexp)
    (define-key map (sps-prefix-keys p "ee")    'sps-eval-last-expression)
    (define-key map (sps-prefix-keys p "ed")    'sps-eval-defun)
    (define-key map (sps-prefix-keys p "er")    'sps-eval-region)
    (define-key map (sps-prefix-keys p "eb")    'sps-eval-buffer)
    (define-key map (sps-prefix-keys p "e SPC") 'sps-eval-dwim)
    ;; Expansion commands
    (define-key map (sps-prefix-keys p "x C-m") 'sps-expand-sexp)
    (define-key map (sps-prefix-keys p "xe")    'sps-expand-last-expression)
    (define-key map (sps-prefix-keys p "xd")    'sps-expand-defun)
    (define-key map (sps-prefix-keys p "xr")    'sps-expand-region)
    (define-key map (sps-prefix-keys p "xb")    'sps-expand-buffer)
    (define-key map (sps-prefix-keys p "x SPC") 'sps-expand-dwim)))

;;;; Scratch buffer

(defun sps-scratch-buffer ()
  "Return the scratch buffer, creating it if necessary."
  (let ((name "*sps-scratch*"))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (lisp-mode)
          (slime-mode t)
          (sps-mode 1)
          (sps-scratch-mode 1)
          (current-buffer)))))

(defun sps-switch-to-scratch-buffer ()
  "Jump to the *sps-scratch* buffer."
  (set-buffer (sps-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(defun sps-scratch ()
  "Jump to the *sps-scratch* buffer."
  (interactive)
  (sps-switch-to-scratch-buffer))

;;;; A little extra SLIME integration

(defun sps-mode-buffer-p (buffer)
  "Return t if `sps-mode` is active in BUFFER."
  (with-current-buffer buffer
    (bound-and-true-p sps-mode)))

(defun sps-recently-visited-sps-buffer ()
  "Return the most recently visited `sps-mode` buffer.
Only considers buffers that are not already visible."
  (or (-first #'(lambda (b) (and (sps-mode-buffer-p b)
                                 (null (get-buffer-window b 'visible))))
              (buffer-list))
      (error "Can't find unshown buffer in sps-mode")))

(defun sps-add-slime-selector-methods ()
  "Add methods to `slime-selector` for `sps-mode` buffers.
Allows access to the most recently visited buffer with `sps-mode`
active via \"p\" and to the *sps-scratch* buffer via \"P\"."
  (interactive)
  (def-slime-selector-method ?p
    "most recently visited buffer using sps-mode."
    (sps-recently-visited-sps-buffer))
  (def-slime-selector-method ?P
    "*sps-scratch* buffer."
    (sps-scratch-buffer)))

;;;; The minor modes

;;;###autoload
(define-minor-mode sps-mode
  "Minor mode for interactively evaluating Parenscript forms."
  :lighter " sps"
  :keymap  sps-mode-map)

;;;###autoload
(define-minor-mode sps-scratch-mode
  "Mode for sps-mode scratch buffer."
  :lighter nil
  :keymap  sps-scratch-mode-map)

(define-minor-mode sps-expansion-mode
  "Minor mode for displaying the code generated by Parenscript."
  :lighter nil
  :keymap  sps-expansion-mode-map)


(provide 'sps-mode)

;; Local Variables:
;; lexical-binding: t
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; sps-mode.el ends here

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
This keymap is initialized empty. To repurpose the keys SLIME
uses for its normal code evaluation and expansion commands, call
`sps-shadow-slime-keybindings!`. Of course, you can alternatively
define your own keybindings in the usual way.")

(defvar sps-expansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "x") 'sps-send-expanded-code)
    map)
  "Keymap for `sps-expansion-mode` buffers.")

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

;;;; Keybindings

(defun sps-shadow-slime-keybindings! ()
  "Rebind SLIME's evaluation keys for Parenscript.

This uses the same keys that `slime-mode` uses, thus shadowing
them. If you want to use both sets of evaluation commands in the
same buffer you will likely want to define other keys manually.

This command must be run in the buffer that the keybindings are
to be used in - it relies on `make-local-variable` to avoid
changing SLIME's bindings in other buffers."
  (interactive)
  (make-local-variable 'slime-mode-map)
  (let ((map slime-mode-map))
    (define-key map (kbd "C-x C-e") nil)
    (define-key map (kbd "C-c C-r") nil)
    (define-key map (kbd "C-M-x")   nil)
    (define-key map (kbd "C-c C-k") nil)
    (define-key map (kbd "C-c RET") nil))
  (let ((map sps-mode-map))
    (define-key map (kbd "C-x C-e") 'sps-eval-last-expression)
    (define-key map (kbd "C-c C-r") 'sps-eval-region)
    (define-key map (kbd "C-M-x")   'sps-eval-defun)
    (define-key map (kbd "C-c C-k") 'sps-eval-buffer)
    (define-key map (kbd "C-c RET") 'sps-expand-sexp)))

;;;; The minor mode

;;;###autoload
(define-minor-mode sps-mode
  "Minor mode for interactively evaluating Parenscript forms."
  :lighter " sps"
  :keymap  sps-mode-map)

;;;; Expansion minor mode

(defun sps-send-expanded-code ()
  "Send the expanded code to the browser."
  (interactive)
  (skewer-eval
   (buffer-substring-no-properties (point-min) (point-max))
   #'skewer-post-minibuffer))

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

;;; trident-mode.el --- Live Parenscript interaction -*- lexical-binding: t -*-

;; Copyright (C) 2013 John Mastro

;; Author: John Mastro <john.b.mastro@gmail.com>
;; Version: 0.0.1
;; Keywords: languages, lisp, processes, tools
;; Package-Requires: ((slime "2013-05-26") (skewer-mode "1.5.0") (dash "1.0.3"))

;;; Commentary:

;; This is an Emacs minor mode and collection of commands for working with
;; Parenscript code in SLIME and sending it to the browser via Skewer. The goal
;; is to create an environment for hacking Parenscript which fits as naturally
;; as possible into the Lisp style of interactive development.

;; See https://github.com/johnmastro/trident-mode.el for additional information.

;; Note that this is very young code and there are certain to be serious
;; problems.

;;; Code:

(require 'slime)
(require 'dash)

;;;; Vars

(defvar trident-mode-map (make-sparse-keymap)
  "Keymap for trident-mode.
This keymap is initialized empty. You can optionally use
`trident-add-keys-with-prefix' to add bindings for all commands
on two-key sequences behind a prefix of your choice.")

(defvar trident-expansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'trident-send-expanded-code)
    (define-key map (kbd "w") 'trident-kill-ring-save-dwim)
    (define-key map (kbd "s") 'write-file)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `trident-expansion-mode' buffers.")

(defvar trident-scratch-mode-map (make-sparse-keymap)
  "Keymap for the *trident-scratch* buffer.")

(defvar trident-expansion-major-mode 'javascript-mode
  "The major mode to enable in expansion buffers.
Note that are currently serious problems when using js2-mode in
expansion buffers. Avoid it for the time being.")

;;;; Code expansion

(defun trident-wrap-in-ps-form (string)
  "Return Parenscript STRING wrapped in a PS:PS form."
  (format "(ps:ps %s)" string))

(defun trident-call-with-expansion (fn string)
  "Call FN on the result of expanding Parenscript STRING.

Note that FN will be called asynchronously and its return value
discarded; it must work through side effects alone.

See also `trident-with-expansion'."
  (let ((string (trident-wrap-in-ps-form string)))
    (slime-eval-async `(swank:eval-and-grab-output ,string)
      #'(lambda (result)
          (funcall fn (read (cadr result))))
      (slime-current-package))))

(defmacro trident-with-expansion (name-and-string &rest body)
  "Expand a Parenscript string and execute BODY.

NAME-AND-STRING should be a two-item list, with the second item
the string to be expanded and the first item the name to which to
bind the result of the expansion. Note that BODY will be executed
asynchronously and its return value discarded; it must work
through side effects alone.

See also `trident-call-with-expansion'."
  (let ((name (car name-and-string))
        (string (cadr name-and-string))
        (rv (make-symbol "rv")))
    `(slime-eval-async
         `(swank:eval-and-grab-output ,(trident-wrap-in-ps-form ,string))
       #'(lambda (,rv)
           (let ((,name (read (cadr ,rv))))
             ,@body))
       (slime-current-package))))

(put 'trident-with-expansion 'lisp-indent-function 1)

(defun trident-expand (string)
  "Display the JavaScript generated from Parenscript STRING.

The resulting JavaScript is displayed in a temporary buffer. The
buffer's major mode is determined by the variable
`trident-expansion-major-mode' (`javascript-mode' by default).
`trident-expansion-mode' is enabled as an additional minor mode."
  (trident-with-expansion (code string)
    (slime-with-popup-buffer ("*Parenscript generated JavaScript*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert code)
      (funcall trident-expansion-major-mode)
      (trident-expansion-mode 1)
      (font-lock-fontify-buffer)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer)))))

(defun trident-compile-buffer-to-file ()
  "Compile the current buffer and write the result.
Prompts for the destination. If a file already exists at the
destination it's overwritten."
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p "Save buffer? "))
    (save-buffer))
  (let* ((this buffer-file-name)
         (dir (and this (file-name-directory this)))
         (initial (and this (concat (file-name-base this) ".js")))
         (destination (read-file-name "Destination: " dir nil nil initial nil))
         (string (buffer-substring-no-properties (point-min) (point-max))))
    (trident-with-expansion (code string)
      (with-temp-buffer
        (erase-buffer)
        (insert code)
        (write-region 1 (point-max) destination)))))

(defun trident-expand-sexp ()
  "Display the expansion of the form at point."
  (interactive)
  (trident-expand (slime-sexp-at-point)))

(defun trident-expand-last-expression ()
  "Display the expansion of the expression preceding point."
  (interactive)
  (trident-expand (slime-last-expression)))

(defun trident-expand-defun ()
  "Display the expansion of the current toplevel form."
  (interactive)
  (trident-expand (slime-defun-at-point)))

(defun trident-expand-region (beg end)
  "Display the expansion of the currently active region."
  (interactive "r")
  (trident-expand (buffer-substring-no-properties beg end)))

(defun trident-expand-buffer ()
  "Display the expansion of the current buffer."
  (interactive)
  (trident-expand-region (point-min) (point-max)))

(defun trident-expand-dwim ()
  "Display the expansion of the active region or toplevel form.
If the region is active this is equivalent to invoking
`trident-expand-region', otherwise it's equivalent to
`trident-expand-defun'."
  (interactive)
  (if (region-active-p)
      (trident-expand-region (region-beginning) (region-end))
    (trident-expand-defun)))

;;;; Code evaluation

(defun trident-eval (string)
  "Compile Parenscript STRING and evaluate it in the browser.

The code is first compiled to JavaScript in the CL image and then
sent to the browser via `skewer-eval'."
  (trident-with-expansion (code string)
    (skewer-eval code #'skewer-post-minibuffer)))

(defun trident-eval-sexp ()
  "Evaluate the expression at point as Parenscript."
  (interactive)
  (trident-eval (slime-sexp-at-point)))

(defun trident-eval-last-expression ()
  "Evaluate the expression preceding point as Parenscript."
  (interactive)
  (trident-eval (slime-last-expression)))

(defun trident-eval-defun ()
  "Evaluate the current toplevel form as Parenscript."
  (interactive)
  (trident-eval (slime-defun-at-point)))

(defun trident-eval-region (beg end)
  "Evaluate the currently active region as Parenscript."
  (interactive "r")
  (trident-eval (buffer-substring-no-properties beg end)))

(defun trident-eval-buffer ()
  "Evaluate the current buffer as Parenscript."
  (interactive)
  (trident-eval-region (point-min) (point-max)))

(defun trident-eval-dwim ()
  "Evaluate the active region or toplevel form.
If the region is active this is equivalent to invoking
`trident-eval-region', otherwise it's equivalent to
`trident-eval-defun'."
  (if (region-active-p)
      (trident-eval-region (region-beginning) (region-end))
    (trident-eval-defun)))

;;;; Expansion buffer commands

(defun trident-send-expanded-code ()
  "Send the expanded code to the browser.
For use from `trident-expansion-mode' buffers."
  (interactive)
  (skewer-eval
   (buffer-substring-no-properties (point-min) (point-max))
   #'skewer-post-minibuffer))

(defun trident-kill-ring-save-dwim ()
  "Save the current buffer's content to the kill ring.
If the region is active, save it; otherwise save the entire
buffer."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (point-min) (point-max))))

;;;; Keybindings

(defun trident-add-keys-with-prefix (p)
  "Add keybindings for `trident-mode' commands behind prefix P."
  (let ((map trident-mode-map)
        (prefix #'(lambda (keys) (read-kbd-macro (concat p " " keys)))))
    ;; Evaluation commands
    (define-key map (funcall prefix "e C-m") 'trident-eval-sexp)
    (define-key map (funcall prefix "e e")   'trident-eval-last-expression)
    (define-key map (funcall prefix "e d")   'trident-eval-defun)
    (define-key map (funcall prefix "e r")   'trident-eval-region)
    (define-key map (funcall prefix "e b")   'trident-eval-buffer)
    (define-key map (funcall prefix "e SPC") 'trident-eval-dwim)
    ;; Expansion commands
    (define-key map (funcall prefix "x C-m") 'trident-expand-sexp)
    (define-key map (funcall prefix "x e")   'trident-expand-last-expression)
    (define-key map (funcall prefix "x d")   'trident-expand-defun)
    (define-key map (funcall prefix "x r")   'trident-expand-region)
    (define-key map (funcall prefix "x b")   'trident-expand-buffer)
    (define-key map (funcall prefix "x SPC") 'trident-expand-dwim)))

;;;; Scratch buffer

(defun trident-scratch-buffer ()
  "Return the scratch buffer, creating it if necessary."
  (let ((name "*trident-scratch*"))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (lisp-mode)
          (slime-mode t)
          (trident-mode 1)
          (trident-scratch-mode 1)
          (current-buffer)))))

(defun trident-switch-to-scratch-buffer ()
  "Jump to the *trident-scratch* buffer."
  (set-buffer (trident-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(defun trident-scratch ()
  "Jump to the *trident-scratch* buffer."
  (interactive)
  (trident-switch-to-scratch-buffer))

;;;; A little extra SLIME integration

(defun trident-mode-buffer-p (buffer)
  "Return t if `trident-mode' is active in BUFFER."
  (with-current-buffer buffer
    (bound-and-true-p trident-mode)))

(defun trident-recently-visited-trident-buffer ()
  "Return the most recently visited `trident-mode' buffer.
Only considers buffers that are not already visible."
  (or (-first #'(lambda (b) (and (trident-mode-buffer-p b)
                                 (null (get-buffer-window b 'visible))))
              (buffer-list))
      (error "Can't find unshown buffer in trident-mode")))

(defun trident-add-slime-selector-methods ()
  "Add methods to `slime-selector' for `trident-mode' buffers.
Allows access to the most recently visited buffer with
trident-mode active via \"p\" and to the *trident-scratch* buffer
via \"P\"."
  (interactive)
  (def-slime-selector-method ?p
    "most recently visited buffer using trident-mode."
    (trident-recently-visited-trident-buffer))
  (def-slime-selector-method ?P
    "*trident-scratch* buffer."
    (trident-scratch-buffer)))

;;;; The minor modes

;;;###autoload
(define-minor-mode trident-mode
  "Minor mode for interactively evaluating Parenscript forms."
  :lighter " tri"
  :keymap  trident-mode-map)

;;;###autoload
(define-minor-mode trident-scratch-mode
  "Mode for trident-mode scratch buffer."
  :lighter nil
  :keymap  trident-scratch-mode-map)

(define-minor-mode trident-expansion-mode
  "Minor mode for displaying the code generated by Parenscript."
  :lighter nil
  :keymap  trident-expansion-mode-map)


(provide 'trident-mode)

;; Local Variables:
;; lexical-binding: t
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; trident-mode.el ends here

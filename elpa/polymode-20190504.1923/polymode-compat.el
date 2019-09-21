;;; polymode-compat.el --- Various compatibility fixes for other packages -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2018, Vitalie Spinu
;; Version: 0.1
;; URL: https://github.com/vitoshka/polymode
;; Keywords: emacs
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode-core)
(require 'advice nil t)

(defgroup polymode-compat nil
  "Polymode compatibility settings."
  :group 'polymode)


;;; emacs 25 compat

(unless (fboundp 'assoc-delete-all)

  (defun assoc-delete-all (key alist &optional test)
    "Delete from ALIST all elements whose car is KEY.
Compare keys with TEST.  Defaults to `equal'.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (unless test (setq test #'equal))
    (while (and (consp (car alist))
	            (funcall test (caar alist) key))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr))
	             (funcall test (caar tail-cdr) key))
	        (setcdr tail (cdr tail-cdr))
	      (setq tail tail-cdr))))
    alist)

  (defun assq-delete-all (key alist)
    "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (assoc-delete-all key alist #'eq)))



;;; Various Wrappers for Around Advice

(defvar *span* nil)

;; advice doesn't provide named symbols. So we need to define specialized
;; wrappers for some key functions (unfinished)
(defmacro pm-define-wrapp-protected (fun)
  "Declare protected function with the name fun--pm-wrapped.
Return new name (symbol). FUN is an unquoted name of a function."
  (let* ((fun-name (symbol-name fun))
         (new-fun (intern (format "%s--pm-wrapped" fun-name)))
         (new-doc (format "  Error Protected function created with `pm-define-protected-wrapp'.\n\n%s"
                          (or (documentation fun) ""))))
    `(progn
       (defun ,new-fun (&rest args)
         ,new-doc
         (condition-case err
             (apply ',fun args)
           (error (message "(%s %s): %s"
                           ,fun-name
                           (mapconcat (lambda (x) (format "%s" x)) args " ")
                           (error-message-string err)))))
       ',new-fun)))

(defun pm-apply-protected (fun args)
  (when fun
    (condition-case-unless-debug err
        (apply fun args)
      (error (message "(%s %s): %s %s"
                      (if (symbolp fun)
                          (symbol-name fun)
                        "anonymous")
                      (mapconcat (lambda (x) (format "%s" x)) args " ")
                      (error-message-string err)
                      ;; (or (and (symbolp fun) "")
                      ;;     (replace-regexp-in-string "\n" "" (format "[%s]" fun)))
                      "[M-x pm-debug-mode RET for more info]")
             nil))))

(defun pm-override-output-position (orig-fun &rest args)
  "Restrict returned value of ORIG-FUN to fall into the current span.
*span* in `pm-map-over-spans` has precedence over span at point.
 ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-innermost-range)))
            (pos (pm-apply-protected orig-fun args)))
        (and pos
             (min (max pos (car range))
                  (cdr range))))
    (apply orig-fun args)))

(defun pm-override-output-cons (orig-fun &rest args)
  "Restrict returned (beg . end) of ORIG-FUN to fall into the current span.
*span* in `pm-map-over-spans` has precedence over span at point.
This will break badly if (point) is not inside expected range.
ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-innermost-range)))
            (be (pm-apply-protected orig-fun args)))
        (let ((out (and be
                        (cons (and (car be)
                                   (min (max (car be) (car range))
                                        (cdr range)))
                              (and (cdr be)
                                   (max (min (cdr be) (cdr range))
                                        (car range)))))))
          out))
    (apply orig-fun args)))

(defun pm-narrowed-override-output-cons (orig-fun &rest args)
  "Restrict returned (beg . end) of ORIG-FUN to fall into the current span.
Run ORIG-FUN with buffer narrowed to span. *span* in
`pm-map-over-spans` has precedence over span at point. ARGS are
passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((*span* (or *span* (pm-innermost-span))))
        (pm-with-narrowed-to-span *span*
          (apply #'pm-override-output-cons orig-fun args)))
    (apply orig-fun args)))

(defun pm-substitute-beg-end (orig-fun beg end &rest args)
  "Execute ORIG-FUN with first BEG and END arguments limited to current span.
*span* in `pm-map-over-spans` has precedence over span at point.
 ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let* ((pos (if (and (<= (point) end) (>=  (point) beg))
                      (point)
                    end))
             (range (or (pm-span-to-range *span*)
                        (pm-innermost-range pos)))
             (new-beg (max beg (car range)))
             (new-end (min end (cdr range))))
        (pm-apply-protected orig-fun (append (list new-beg new-end) args)))
    (apply orig-fun beg end args)))

(defun pm-execute-narrowed-to-span (orig-fun &rest args)
  "Execute ORIG-FUN narrowed to the current span.
*span* in `pm-map-over-spans` has precedence over span at point.
 ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (pm-with-narrowed-to-span *span*
        (pm-apply-protected orig-fun args))
    (apply orig-fun args)))


;;; Flyspel
(defun pm--flyspel-dont-highlight-in-chunkmodes (beg end _poss)
  (or (car (get-text-property beg :pm-span))
      (car (get-text-property end :pm-span))))


;;; C/C++/Java
(pm-around-advice 'c-before-context-fl-expand-region #'pm-override-output-cons)
;; (advice-remove 'c-before-context-fl-expand-region #'pm-override-output-cons)
(pm-around-advice 'c-state-semi-safe-place #'pm-override-output-position)
;; (advice-remove 'c-state-semi-safe-place #'pm-override-output-position)
;; c-font-lock-fontify-region calls it directly
;; (pm-around-advice 'font-lock-default-fontify-region #'pm-substitute-beg-end)
(pm-around-advice 'c-determine-limit #'pm-execute-narrowed-to-span)


;;; Python
(declare-function pm--first-line-indent "polymode-methods")
(defun pm--python-dont-indent-to-0 (fun)
  "Fix indent FUN not to cycle to 0 indentation."
  (if (and polymode-mode pm/type)
      (let ((last-command (unless (eq (pm--first-line-indent) (current-indentation))
                            last-command)))
        (funcall fun))
    (funcall fun)))

(pm-around-advice 'python-indent-line-function #'pm--python-dont-indent-to-0)


;;; Core Font Lock
(defvar font-lock-beg)
(defvar font-lock-end)
(defun pm-check-for-real-change-in-extend-multiline (fun)
  "Protect FUN from inf-looping at ‘point-max’.
FUN is `font-lock-extend-region-multiline'. Propagate only real
changes."
  ;; fixme: report this ASAP!
  (let ((obeg font-lock-beg)
        (oend font-lock-end)
        (change (funcall fun)))
    (and change
         (not (eq obeg font-lock-beg))
         (not (eq oend font-lock-end)))))

(pm-around-advice 'font-lock-extend-region-multiline #'pm-check-for-real-change-in-extend-multiline)


;;; Editing

;; (pm-around-advice 'fill-paragraph #'pm-execute-narrowed-to-span)
;; (advice-remove 'fill-paragraph #'pm-execute-narrowed-to-span)


;; Synchronization of points does not work always as expected because some low
;; level functions move indirect buffers' points when operate in the base
;; buffer. See comment in `polymode-with-current-base-buffer'.

;; (defun polymode-with-save-excursion (orig-fun &rest args)
;;   "Execute ORIG-FUN surrounded with `save-excursion'.
;; This function is intended to be used in advises of functions
;; which modify the buffer in the background and thus trigger
;; `pm-switch-to-buffer' on next post-command hook in a wrong place.
;; ARGS are passed to ORIG-FUN."
;;   (if polymode-mode
;;       (save-excursion
;;         (apply orig-fun args))
;;     (apply orig-fun args)))
;;
;; `save-buffer` misbehaves because after each replacement modification hooks
;; are triggered and poly buffer is switched in unpredictable fashion (#93).
;; This happens because basic-save-buffer uses save-buffer but not
;; save-excursion. Thus if base and indirect buffer don't have same point, at
;; the end of the function inner buffer will have the point from the base
;; buffer. Can be reproduced with (add-hook 'before-save-hook
;; 'delete-trailing-whitespace nil t) in the base buffer.
;;
;; (pm-around-advice 'basic-save-buffer #'polymode-with-save-excursion)
;; (advice-remove 'basic-save-buffer #'polymode-with-save-excursion)

;; Query replace were probably misbehaving due to unsaved match data (#92). The
;; following is probably not necessary. (pm-around-advice 'perform-replace
;; 'pm-execute-inhibit-modification-hooks)


;;; DESKTOP SAVE #194

;; NB: desktop-save saves indirect buffers as base buffers but assumes that
;; buffer names are the same. This would be ok if we hide implementation buffers
;; as per #34.

(defun polymode-fix-desktop-buffer-info (fn buffer)
  "Save polymode buffers without mode prefix."
  (let ((out (funcall fn buffer))
        (base (buffer-base-buffer)))
    (with-current-buffer buffer
      (if (not (and polymode-mode base))
          out
        (when (car out)
          (setf (car out) (buffer-name base)))
        (setf (nth 2 out) (buffer-name base))
        out))))

(declare-function desktop-buffer-info "desktop")
(with-eval-after-load "desktop"
  (advice-add #'desktop-buffer-info :around #'polymode-fix-desktop-buffer-info))


;;; MATLAB #199

;; matlab-mode is an old non-standard mode which doesn't trigger
;; `after-change-major-mode-hook`. As a result polymode cannot detect that
;; font-lock-mode is on and sets the `poly-lock-allow-fontification` to nil.
;; Explicitly trigger font-lock as a workaround.
(add-hook 'matlab-mode-hook (lambda () (font-lock-mode t)))


;;; EVIL

(declare-function evil-change-state "evil-core")
(defun polymode-switch-buffer-keep-evil-state-maybe (old-buffer new-buffer)
  (when (and (boundp 'evil-state)
             evil-state)
    (let ((old-state (buffer-local-value 'evil-state old-buffer))
          (new-state (buffer-local-value 'evil-state new-buffer)))
      (unless (eq old-state new-state)
        (with-current-buffer new-buffer
          (evil-change-state old-state))))))

(eval-after-load 'evil-core
  '(add-hook 'polymode-after-switch-buffer-hook 'polymode-switch-buffer-keep-evil-state-maybe))


;;; HL line

(defvar hl-line-mode)
(defvar global-hl-line-mode)
(declare-function hl-line-unhighlight "hl-line")
(declare-function global-hl-line-unhighlight "hl-line")
(add-to-list 'polymode-move-these-minor-modes-from-old-buffer 'hl-line-mode)
(defun polymode-switch-buffer-hl-unhighlight (old-buffer _new-buffer)
  (with-current-buffer old-buffer
    ;; We are moving hl-line-mode already
    (when hl-line-mode
      (hl-line-unhighlight))
    (when global-hl-line-mode
      (global-hl-line-unhighlight))))
(eval-after-load 'hl-line
  '(add-hook 'polymode-after-switch-buffer-hook 'polymode-switch-buffer-hl-unhighlight))


;;; YAS

(with-eval-after-load "yasnippet"
  (add-hook 'yas-before-expand-snippet-hook #'polymode-disable-post-command)
  (add-hook 'yas-after-exit-snippet-hook #'polymode-enable-post-command))

(provide 'polymode-compat)
;;; polymode-compat.el ends here

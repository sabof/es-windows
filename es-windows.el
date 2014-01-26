;;; es-windows.el --- Window-management utilities

;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/es-windows
;;; Package-Requires: ((cl-lib "0.3") (es-lib "0.3"))

;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-windows
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'quail)
(require 'cl-lib)
(require 'es-lib)


(defface esw/label-face
    `((t (:inherit font-lock-function-name-face
                   :height ,(* 2 (face-attribute 'default :height)))))
  "Face used for regular files in project-explorer sidebar."
  :group 'es-windows)

(defcustom esw/help-message "
Each number/letter represents an emacs window.
Windows followed by H or V, are internal Horizontal or Vertical splitters.
The last window is an external window, showing this buffer.
Type the number/letter of the window you want, followed by
^, >, v, <, in which case the window will be split in that direction
or RET, in which case the window itself will be used.

To prevent this message from showing, set `esw/help-message' to `nil'"
  "Instructions for esw."
  :group 'es-windows
  :type 'sexp)

(defun esw/window-children (window)
  (let* (( first-child (or (window-left-child window)
                           (window-top-child window)))
         ( children (list first-child)))
    (when first-child
      (while (window-next-sibling (car children))
        (push (window-next-sibling (car children))
              children))
      (nreverse children))))

(cl-defun esw/window-lineage (&optional (window (selected-window)))
  "Result includes WINDOW"
  (cl-loop for the-window = window then (window-parent the-window)
           while the-window
           collecting the-window))

(defun esw/shortcuts ()
  (cl-remove-if (lambda (it) (member it '("V" "v")))
                (cl-loop with layout = (split-string quail-keyboard-layout "")
                         for row from 1 to 4
                         nconc (cl-loop for col from 1 to 10
                                        collect (nth (+ 1 (* 2 col) (* 30 row)) layout)))))

(cl-defun esw/internal-window-list (&optional (window (frame-root-window)))
  (let (( result (list window))
        ( fringe (list window))
        new-fringe)
    (while fringe
      (cl-dolist (window fringe)
        (let ((children (esw/window-children window)))
          (when children
            (cl-callf append result children)
            (cl-callf append new-fringe children))))
      (setq fringe new-fringe
            new-fringe nil))
    result))

(defun esw/cover-window (window label)
  (let ((buffer (generate-new-buffer "esw")))
    (with-current-buffer buffer
      ;; (face-remap-add-relative 'default 'esw/label-face)
      (insert label)
      (setq cursor-type nil)
      (when (window-dedicated-p window)
        (set-window-dedicated-p window nil))
      (set-window-buffer window buffer)
      buffer)))

(defun esw/window-type (window)
  (cond ( (window-left-child window)
          "H")
        ( (window-top-child window)
          "V")))

(defun esw/window-list ()
  (cl-remove-if (lambda (win) (window-parameter win 'window-side))
                (window-list nil nil (frame-first-window))))

(defun esw/save-windows ()
  (let ((windows (esw/window-list)))
    (list (current-window-configuration)
          (cl-remove-if-not 'window-dedicated-p windows)
          (mapcar (lambda (window) (cons window (window-point window)))
                  windows)
          (cl-remove-if-not (lambda (window)
                              (with-selected-window window
                                (eobp)))
                            windows)
          )))

(define-minor-mode esw/minibuffer-mode
    "Custom esw keybindings for the minibuffer."
  nil nil
  (let ((map (make-sparse-keymap)))
    (cl-dolist (key '("v" "V" "<" ">" "^"))
      (define-key map key 'self-insert-and-exit))
    map))

(defun esw/restore-windows (spec)
  (cl-destructuring-bind
      (config dedicated-windows window-points eobp-window-list)
      spec
    (set-window-configuration config)
    (cl-dolist (w window-points)
      (set-window-point (car w) (cdr w)))
    (cl-dolist (w dedicated-windows)
      (set-window-dedicated-p w t))
    (cl-dolist (win eobp-window-list)
      (with-selected-window win
        (with-current-buffer (window-buffer win)
          (goto-char (point-max)))))))

(defun esw/select-window (&optional prompt)
  (interactive)
  (let* (( spec (esw/save-windows))
         ( windows (esw/window-list))
         ( ids (esw/shortcuts))
         ( window-id-map (cl-mapcar (lambda (window id)
                                      (cons window id))
                                    (esw/internal-window-list)
                                    ids))
         buffers
         user-input
         user-input-action
         selected-window)
    (unwind-protect
         (progn (setq buffers
                      (mapcar (lambda (window)
                                (esw/cover-window
                                 window
                                 (concat (mapconcat
                                          (lambda (window)
                                            (concat
                                             (propertize (cdr (assoc window window-id-map))
                                                         'face 'esw/label-face)
                                             (esw/window-type window)))
                                          (esw/window-lineage window)
                                          " ")
                                         esw/help-message)))
                              windows))
                (let* (( minibuffer-setup-hook
                         (cons 'esw/minibuffer-mode
                               minibuffer-setup-hook)))
                  (setq user-input (read-string (or prompt "Select window: "))))
                (string-match "^\\(.+?\\)\\([Vv<>^]\\)?$" user-input)
                (setq selected-window (car (rassoc (match-string 1 user-input) window-id-map)))
                (setq user-input-action (match-string 2 user-input))
                ;; (message "%s -- %s" selected-window user-input-action)
                )
      (mapc 'kill-buffer buffers)
      (esw/restore-windows spec))
    (when selected-window
      (when user-input-action
        (cl-callf downcase user-input-action)
        (setq selected-window
              (split-window
               selected-window
               nil (cdr (assoc user-input-action
                               '((">" . right)
                                 ("<" . left)
                                 ("^" . above)
                                 ("v" . below)
                                 ))))))
      (select-window selected-window)
      (cl-dolist (win (nth 3 spec))
        (with-selected-window win
          (with-current-buffer (window-buffer win)
            (goto-char (point-max)))))
      selected-window)))

(defun esw/show-buffer (buffer)
  (set-window-buffer (esw/select-window)
                     buffer))

(provide 'es-windows)
;;; es-windows.el ends here

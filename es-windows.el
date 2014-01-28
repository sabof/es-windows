;;; es-windows.el --- Window-management utilities -*- lexical-binding: t -*-

;;; Version: 0.2
;;; Author: sabof
;;; URL: https://github.com/sabof/es-windows
;;; Package-Requires: ((cl-lib "0.3") (emacs "24"))

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

(require 'cl-lib)
(require 'face-remap)

(defgroup es-windows nil
  "A project explorer sidebar."
  :group 'convenience)

(defface esw/label-face
    `((t (:inherit font-lock-function-name-face
                   :height ,(* 2 (face-attribute 'default :height)))))
  "Face used for regular files in project-explorer sidebar."
  :group 'es-windows)

(defface esw/selection-face
    `((t (:inherit region)))
  "Face used for regular files in project-explorer sidebar."
  :group 'es-windows)

(defcustom esw/be-helpful t
  "Whether to show help messages."
  :group 'es-windows
  :type 'boolean)

(defcustom esw/colorize-selection t
  "Whether to dynamically colorize the selected window."
  :group 'es-windows
  :type 'boolean)

(defcustom esw/key-direction-mappings
  '((">" . right)
    ("<" . left)
    ("^" . above)
    ("v" . below))
  "Keys that will trigger splitting."
  :group 'es-windows
  :type 'sexp)

(defun esw/user-input-regex ()
  (let ((keys (if (char-equal (aref (caar esw/key-direction-mappings) 0) ?^ )
                  (mapconcat 'car (reverse esw/key-direction-mappings) "")
                (mapconcat 'car esw/key-direction-mappings ""))))
    (format "^ *\\([^%s]+\\)?\\([%s]\\)? *$"
            keys keys)))

(defvar esw/window-id-mappings nil)

(defvar esw/help-message "
Each number represents an emacs window. Windows followed by H or V, are
internal Horizontal or Vertical splitters. The last window is an external
window, showing this buffer.

Type the number of the window you want, followed by RET, and that window will be
used. You can also type %s instead of RET, in which case the window
will be split in that direction.

If no window is provided, use the closest to root window that can be split.

To prevent this message from showing, set `esw/be-helpful' to `nil'")

(defun esw/window-children (window)
  (let* (( first-child (or (window-left-child window)
                           (window-top-child window)))
         ( children (list first-child)))
    (when first-child
      (while (window-next-sibling (car children))
        (push (window-next-sibling (car children))
              children))
      (nreverse children))))

(defun esw/shortcuts ()
  (let (( keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                 "q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
                 "a" "s" "d" "f" "g" "h" "j" "k" "l" ";"
                 "z" "x" "c" "b" "n" "m" "," "." "/"))
        ( action-keys (mapcar 'car esw/key-direction-mappings)))
    (cl-remove-if (lambda (it) (member it action-keys))
                  keys)))

(defun esw/window-splittable-p (window)
  (cond ( (window-parameter window 'window-side)
          nil)
        ( (cl-some (lambda (window)
                     (window-parameter window 'window-side))
                   (esw/window-children window))
          nil)
        ( t)))

(cl-defun esw/window-lineage (&optional (window (selected-window)))
  "Result includes WINDOW"
  (nreverse
   (cl-loop for the-window = window then (window-parent the-window)
            while the-window
            when (esw/window-splittable-p the-window)
            collect the-window)))

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
    (cl-remove-if-not 'esw/window-splittable-p
                      result)
    ))

(defun esw/window-list ()
  (cl-remove-if-not 'esw/window-splittable-p
                    (window-list nil nil (frame-first-window))))

(defun esw/cover-window (window label)
  (let (( buffer (generate-new-buffer
                  (buffer-name
                   (window-buffer window)))))
    (with-current-buffer buffer
      (setq major-mode 'esw/cover-mode)
      (insert label)
      (goto-char (point-min))
      (setq cursor-type nil)
      (setq buffer-read-only t)
      (when (window-dedicated-p window)
        (set-window-dedicated-p window nil))
      (set-window-buffer window buffer)
      buffer)))

(defun esw/window-type (window)
  (cond ( (window-left-child window)
          "H")
        ( (window-top-child window)
          "V")))

(defun esw/save-windows ()
  (let ((windows (esw/window-list)))
    (list (current-window-configuration)
          (cl-remove-if-not 'window-dedicated-p windows)
          (mapcar (lambda (window) (cons window (window-point window)))
                  windows)
          (cl-remove-if-not 'esw/window-eobp windows)
          )))

(cl-defun esw/mark-windows ()
  (let* (( input-string
           (save-excursion
             ;; (setq tmp (buffer-string))
             (goto-char (length (minibuffer-prompt)))
             ;; (search-forward ": " )
             (buffer-substring (point) (point-max))))
         ( buffer-id (progn (string-match (esw/user-input-regex)
                                          input-string)
                            (match-string 1 input-string)))
         ( selected-window
           (car (rassoc buffer-id esw/window-id-mappings)))
         ( windows-to-mark
           (when selected-window
             (cl-remove-if 'esw/window-children
                           (esw/internal-window-list
                            selected-window)))))
    (cl-dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (eq major-mode 'esw/cover-mode)
          (if (memq window windows-to-mark)
              (face-remap-add-relative
               'default 'esw/selection-face)
            (face-remap-remove-relative
             '(default . esw/selection-face))
            ))))))

(defvar esw/minibuffer-mode-map nil)
(define-minor-mode esw/minibuffer-mode
    "Custom esw keybindings for the minibuffer."
  nil nil nil
  (setq esw/minibuffer-mode-map
        (let ((map (make-sparse-keymap)))
          (cl-dolist (mapping esw/key-direction-mappings)
            (define-key map (kbd (car mapping)) 'self-insert-and-exit))
          map))
  (when esw/colorize-selection
    (add-hook 'post-command-hook 'esw/mark-windows nil t)))

;; Test:
;; while true; do date; sleep 0.3; done

(defun esw/window-goto-eob (window)
  (when (window-live-p window)
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (goto-char (point-max))))))

(defun esw/window-eobp (window)
  (when (window-live-p window)
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (eobp)))))

(defun esw/restore-windows (spec)
  (cl-destructuring-bind
      (config dedicated-windows window-points eobp-window-list)
      spec
    (set-window-configuration config)
    (cl-dolist (window window-points)
      (set-window-point (car window) (cdr window)))
    (cl-dolist (window dedicated-windows)
      (set-window-dedicated-p window t))
    (mapc 'esw/window-goto-eob eobp-window-list)))

(cl-defun esw/select-window (&optional prompt no-splits)
  (interactive)
  (unless prompt
    (setq prompt
          (if (and (not no-splits) esw/be-helpful)
              (format "Select a window (type a large number followed by %s or RET): "
                      (mapconcat 'car esw/key-direction-mappings ", "))
            "Select window: ")))
  (let* (( spec (esw/save-windows))
         ( windows (esw/window-list))
         ( internal-windows (esw/internal-window-list))
         ( esw/window-id-mappings
           (cl-mapcar 'cons internal-windows (esw/shortcuts)))
         ( segment-label
           (lambda (window)
             (concat
              (propertize (cdr (assoc window
                                      esw/window-id-mappings))
                          'face 'esw/label-face)
              (esw/window-type window))))
         ( cover-window
           (lambda (window)
             (esw/cover-window
              window
              (if no-splits
                  (assoc window esw/window-id-mappings)
                (concat (mapconcat segment-label
                                   (esw/window-lineage window)
                                   " ")
                        (when esw/be-helpful
                          (format esw/help-message
                                  (mapconcat 'car esw/key-direction-mappings ", "))))))))
         buffers
         user-input-split
         selected-window)

    (unwind-protect
         (let (( minibuffer-setup-hook
                 (cons 'esw/minibuffer-mode minibuffer-setup-hook))
               user-input)
           (setq buffers (mapcar cover-window windows))
           (if no-splits
               ;; FIXME: Implement me
               (progn)
             (setq user-input (read-string prompt))
             (string-match (esw/user-input-regex) user-input))
           (setq selected-window
                 (if (match-string 1 user-input)
                     (or (car (rassoc (match-string 1 user-input)
                                      esw/window-id-mappings))
                         (user-error "Not a valid window"))
                   (car internal-windows)))
           (setq user-input-split (match-string 2 user-input)))
      (cl-dolist (buffer buffers)
        (ignore-errors
          (kill-buffer buffer)))
      (esw/restore-windows spec))

    (if user-input-split
        (setq selected-window
              (split-window selected-window
                            nil
                            (cdr (assoc user-input-split
                                        esw/key-direction-mappings))))
      (while (esw/window-children selected-window)
        (let ((children (esw/window-children selected-window)))
          (mapc 'delete-window (cl-rest children))
          (setq selected-window (car children)))
        (set-window-dedicated-p selected-window nil)))
    (select-window selected-window)
    selected-window))

(defun esw/show-buffer (buffer)
  (interactive (list (get-buffer-create (read-buffer "Choose buffer: "))))
  (set-window-buffer (esw/select-window) buffer))

(defun esw/move-window (window)
  "Show current buffer in a different window, and delete the old window."
  (interactive (list (selected-window)))
  (unless (eq t (window-deletable-p window))
    (user-error "Can't delete window"))
  (let* (( ori-window window)
         ( buffer (window-buffer ori-window))
         ( new-window (esw/select-window)))
    (unless (eq new-window ori-window)
      (set-window-buffer new-window buffer)
      (delete-window ori-window))))

(provide 'es-windows)
;;; es-windows.el ends here

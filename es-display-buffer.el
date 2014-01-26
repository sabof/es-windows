(require 'quail)
(require 'cl-lib)
(require 'es-lib)


(defface edb-label-face
    `((t (:inherit font-lock-function-name-face
                   :height ,(* 2 (face-attribute 'default :height)))))
  "Face used for regular files in project-explorer sidebar."
  :group 'project-explorer)


(defun edb-window-children (window)
  (let* (( first-child (or (window-left-child window)
                           (window-top-child window)))
         ( children (list first-child)))
    (when first-child
      (while (window-next-sibling (car children))
        (push (window-next-sibling (car children))
              children))
      (nreverse children))))

(defun edb-shortcuts ()
  (cl-remove-if (lambda (it) (member it '("V" "v")))
                (cl-loop with layout = (split-string quail-keyboard-layout "")
                         for row from 1 to 4
                         nconc (cl-loop for col from 1 to 10
                                        collect (nth (+ 1 (* 2 col) (* 30 row)) layout)))))

(cl-defun edb-internal-window-tree (&optional (window (frame-root-window)))
  (let (( children (edb-window-children window)))
    (if children
        (cons window (mapcar 'edb-internal-window-tree children))
      window)))

(cl-defun edb-internal-window-list (&optional (window (frame-root-window)))
  (let (( result (list window))
        ( fringe (list window))
        new-fringe
        ( children (edb-window-children window)))
    (while fringe
      (cl-dolist (window fringe)
        (let ((children (edb-window-children window)))
          (when children
            (cl-callf nconc result children)
            (cl-callf nconc new-fringe children))))
      (setq fringe new-fringe
            new-fringe nil))
    result))

(defun edb-cover-window (window label)
  (let ((buffer (generate-new-buffer "edb")))
    (with-current-buffer buffer
      (face-remap-add-relative 'default 'edb-label-face)
      (insert label)
      (setq cursor-type nil)
      (when (window-dedicated-p window)
        (set-window-dedicated-p window nil))
      (set-window-buffer window buffer)
      buffer)))

(defun edb-window-type (window)
  (cond ( (window-left-child window)
          "H")
        ( (window-top-child window)
          "V")))

(defun edb-window-label-1 (window)
  (let (( string (prin1-to-string window))
        ( type (cond ( (window-left-child window)
                       "H-")
                     ( (window-top-child window)
                       "V-")
                     ( t "")))
        id)
    (string-match "^#<window \\([[:digit:]]+\\)" string)
    (setq id (match-string 1 string))
    (concat type id)))

(defun edb-window-label ()
  (mapconcat 'edb-window-label-1
             (es-window-lineage)
             " "))

(defun edb-window-list ()
  (cl-remove-if (lambda (win) (window-parameter win 'window-side))
                (window-list nil nil (frame-first-window))))

(defun edb-save-windows ()
  (let ((windows (edb-window-list)))
    (list (current-window-configuration)
          (cl-remove-if-not 'window-dedicated-p windows)
          (mapcar (lambda (window) (cons window (window-point window)))
                  windows)
          (cl-remove-if-not (lambda (window)
                              (with-selected-window window
                                (eobp)))
                            windows)
          )))

(defun edb-restore-windows (spec)
  (cl-destructuring-bind
      (config dedicated-windows window-points eobp-window-list)
      spec
    (set-window-configuration config)
    (cl-dolist (w window-points)
      (set-window-point (car w) (cdr w)))
    (cl-dolist (w dedicated-windows)
      (set-window-dedicated-p w t))
    (cl-loop for win in eobp-window-list
             do (with-selected-window win
                  (goto-char (point-max))))))

(defun edb-prompt-for-window (prompt)
  (interactive (list "Select window: "))
  (let* (( spec (edb-save-windows))
         ( windows (edb-window-list))
         ( ids (edb-shortcuts))
         ( window-id-map (cl-mapcar (lambda (window id)
                                      (cons window id))
                                    (edb-internal-window-list)
                                    ids))
         buffers
         user-input
         user-input-action
         selected-window)
    (unwind-protect
         (progn (setq buffers
                      (mapcar (lambda (window)
                                (edb-cover-window
                                 window
                                 (mapconcat (lambda (window)
                                              (concat
                                               (cdr (assoc window window-id-map))
                                               (edb-window-type window)))
                                            (es-window-lineage window)
                                            " ")))
                              windows))
                (let* (( setup-custom-bindings
                         (lambda ()
                           (es-buffer-local-set-keys
                             (kbd "v") 'self-insert-and-exit
                             (kbd "V") 'self-insert-and-exit
                             (kbd ">") 'self-insert-and-exit
                             (kbd "<") 'self-insert-and-exit
                             (kbd "^") 'self-insert-and-exit)))
                       ( minibuffer-setup-hook
                         (cons setup-custom-bindings
                               minibuffer-setup-hook)))
                  (setq user-input (read-string prompt)))
                (string-match "^\\(.+?\\)\\([Vv<>^]\\)?$" user-input)
                (setq selected-window (car (rassoc (match-string 1 user-input) window-id-map)))
                (setq user-input-action (match-string 2 user-input))
                ;; (message "%s -- %s" selected-window user-input-action)
                )
      (mapc 'kill-buffer buffers)
      (edb-restore-windows spec))
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
      selected-window)))

(provide 'es-display-buffer)
;;; es-display-buffer.el ends here

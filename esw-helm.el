;;; esw-helm.el -- Helm support for es-windows                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  kassick

;; Author: kassick <kassick@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun helm-esw/show-buffer (candidate)
  (set-window-buffer (esw/select-window nil t t) (get-buffer candidate)))

(defun helm-esw/find-file (candidate)
  (set-window-buffer (esw/select-window nil t t) (find-file-noselect candidate)))

(defun helm-esw/run-show-buffer ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/show-buffer)))

(defun helm-esw/run-find-file ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/find-file)))

(eval-after-load 'helm-files
  (lambda ()
    (if (locate-library "es-windows")
        (progn
          ;; Add description
          (add-to-list 'helm-find-files-actions
                       '("Find file in in new splited window `C-c C-w'" . helm-esw/find-file ) t)
          ;; Bind C-c C-w
          (define-key helm-find-files-map (kbd "C-c C-w") 'helm-esw/run-find-file)))))

(eval-after-load 'helm-buffers
  (lambda ()
    (if (locate-library "es-windows")
        (progn
          ;; Add description
          (add-to-list 'helm-type-buffer-actions
                       '("Display buffer(s) in new splited window `C-c C-w'" . helm-esw/show-buffer) t)
          ;; Bind C-c C-w
          (define-key helm-buffer-map (kbd "C-c C-w") 'helm-esw/run-show-buffer)))))

(eval-after-load 'helm-projectile
  (lambda ()
    (if (locate-library "es-windows")
        (define-key helm-projectile-find-file-map (kbd "C-c C-w") 'helm-esw/run-find-file))))


(provide 'esw-helm.el)
;; esw-helm.el ends here

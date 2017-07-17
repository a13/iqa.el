;;; iqa.el --- Init file(and directory) Quick Access. -*- lexical-binding: t -*-

;; Homepage: https://github.com/a13/iqa.el
;; Package-Requires: ((emacs "24.3"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Sadly, Emacs (unlike Spacemacs) doesn't have functions to open it's own init
;; file, so thousands of users have to write their owns.  I'm not different :)
;;
;; `iqa-find-user-init-file' is a shorthand to open user init file.
;; By default `user-init-file' is used.  If your configuration is generated
;; from org-mode source you may want to point it to your org file.
;;
;; (setq iqa-user-init-file (concat user-emacs-directory "init.org"))
;;
;; File is opened by `find-file', but you can redefine it by e.g.
;;
;; (setq iqa-find-file-function #'find-file-other-window)
;;
;; `iqa-find-user-init-directory' opens init file directory
;;
;; `iqa-setup-default' binds "C-c f" to `iqa-find-user-init-file',
;; "C-c d" to `iqa-find-user-init-directory'
;;
;;
;; Installation with `quelpa-use-package':
;;
;; (use-package iqa
;;   :ensure nil
;;   :quelpa
;;   (iqa :repo "a13/iqa.el" :fetcher github :version original)
;;   :init
;;   (setq iqa-user-init-file (concat user-emacs-directory "init.org"))
;;   :config
;;   (iqa-setup-default))

;;; Code:

(defvar iqa-user-init-file
  nil
  "Default init file to open instead of `user-init-file'.")

(defvar iqa-find-file-function
  #'find-file
  "Find file function.  Should take one required FILENAME argument.")

(defun iqa--init-file ()
  "Return init file name."
  (or iqa-user-init-file user-init-file))

(defun iqa-find-user-init-file ()
  "Open user init file using iqa-find-file-function."
  (interactive)
  (funcall iqa-find-file-function (iqa--init-file)))

(defun iqa-find-user-init-directory ()
  "Open a directory containing `iqa-user-init-file' or `user-init-file'."
  (interactive)
  (funcall iqa-find-file-function (file-name-directory (iqa--init-file))))

;;;###autoload
(defun iqa-setup-default ()
  "Setup default shortcuts for `iqa-find-user-init-file'/`iqa-find-user-init-directory'."
  (interactive)
  (define-key global-map (kbd "C-c f") #'iqa-find-user-init-file)
  (define-key global-map (kbd "C-c d") #'iqa-find-user-init-directory))

(provide 'iqa)

;;; iqa.el ends here

(defconst *home-dir* "/workspaces/acl2ml/")
(defconst *acl2-dir* "/home/acl2/saved_acl2")



(defconst *weka-dir* (concat *home-dir* "weka.jar"))
(defconst *acl2-buffer-name* "*acl2*")

(defun acl2-extraction-mode ()
  (progn
    (load-file (concat *home-dir* "code/extraction.el"))
    (load-file (concat *home-dir* "code/table-to-feature-vector.el"))
    (load-file (concat *home-dir* "code/extraction-recursive.el"))
    (load-file (concat *home-dir* "code/storage.el"))
    (load-file (concat *home-dir* "code/shortcuts.el"))
    (load-file (concat *home-dir* "code/weka-connection.el"))
    (load-file (concat *home-dir* "code/menus.el"))
    (load-file (concat *home-dir* "code/used-lemmas.el"))
    (load-file (concat *home-dir* "code/guards.el"))
    ))


(add-to-list 'auto-mode-alist
             '("\\.lisp\\'" . (lambda ()
			     (progn (acl2-as-lisp-mode) (acl2-extraction-mode))
                               )))




(defun start-acl2ml ()
  (interactive)
  (save-excursion 
    (acl2ml-init-acl2)
  (init-guards) (init-testing)
  (activate-icons)
  (exported-libraries)
  (with-current-buffer "*Shell Command Output*"
    (switch-to-buffer-other-window "*acl2*")
    ))
  )


(defun acl2-as-lisp-mode ()
  (progn (lisp-mode)
	 (set (make-local-variable 'inferior-lisp-buffer) "*acl2*")))


(defun acl2ml-init-acl2 ()
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (require 'comint)
  (switch-to-buffer (make-comint "acl2" *acl2-dir*))
  (require 'inf-lisp)
  (inferior-lisp-mode)
  (other-window 1)
  )
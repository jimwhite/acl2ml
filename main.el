;; ACL2(ml) - Machine Learning for ACL2
;; 
;; To use ACL2(ml), add the following line to your .emacs file:
;; (load-file "/workspaces/acl2ml/main.el")
;;
;; Or if you have ACL2(ml) installed elsewhere, update the path accordingly:
;; (load-file "/path/to/your/acl2ml/main.el")
;;
;; This will automatically load ACL2(ml) when you open .lisp files.
;; To manually start ACL2(ml), use: M-x start-acl2ml

(defconst *home-dir* "/workspaces/acl2ml/")
(defconst *acl2-dir* "/home/acl2/saved_acl2")



(defconst *weka-dir* (concat *home-dir* "weka.jar"))
(defconst *acl2-buffer-name* "*inferior-acl2*")

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
	 (set (make-local-variable 'inferior-lisp-buffer) "*inferior-acl2*")))


(defun acl2ml-init-acl2 ()
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (require 'comint)
  (load-file "/home/acl2/books/interface/emacs/acl2-mode.el")
  (load-file "/home/acl2/books/interface/emacs/inf-acl2.el")
  (setq inferior-acl2-program *acl2-dir*)
  (inferior-acl2 inferior-acl2-program)
  (other-window 1)
  )
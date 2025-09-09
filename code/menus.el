(easy-menu-define ml4acl2-menu global-map "ACL2(ml)"
  '("ACL2(ml)"
    ("Algorithm"
       ["K-means" (change-algorithm "k")
	:selected (string= algorithm "k")
	:style toggle
	:help "Use k-means algorithm"]
       ["EM" (change-algorithm "e")
	:selected (string= algorithm "e")
	:style toggle
	:active (string= ml-system "w")
	:help "Use Simple EM algorithm"]
       ["FarthestFirst" (change-algorithm "f")
	:selected (string= algorithm "f")
	:style toggle
	:active (string= ml-system "w")
	:help "Use FarhestFirst algorithm"])
    ("Granularity"
       ["1"  (change-granularity 1)
	:selected (eq granularity-level 1)
	:style toggle
	:help "Low granularity (big clusters - weak correlation)"]
       ["2"  (change-granularity 2)
	:selected (eq granularity-level 2)
	:style toggle]
       ["3"  (change-granularity 3)
	:selected (eq granularity-level 3)
	:style toggle
	:help "Medium granularity"]
       ["4"  (change-granularity 4)
	:selected (eq granularity-level 4)
	:style toggle]
       ["5"  (change-granularity 5)
	:selected (eq granularity-level 5)
	:style toggle
	:help "High granularity (small clusters - strong correlation)"])
    ("Additional features"
     ["Explain cluster similarities" (change-whysimilar)
      :selected whysimilar
      :style toggle
      :help "Explain the features which are used to group the clusters"]
     )
    ["Clusters" (cluster-general)
     :keys "C-c C-c"]
    ["Obtain Guards of Theorem" (obtain-guards-theorem)
     :keys "C-c C-g"
     :help "Put the cursor at the beggining of a theorem and this option will show you the guards"]
    ["Show similarities" (show-similarities-general)
     :keys "C-c C-s"
     :help "Put the cursor at the beggining of a theorem (or definition) and this option will show you the similar theorems (or definitions)"]   
    ["Export library" (export-library)
     :keys "C-c C-e"]))


(defvar acl2-search-library nil)


(defun change-acl2-search-library ()
  (setf acl2-search-library (not acl2-search-library)))



(defvar lemmaanalogy nil)

(defun change-lemma-analogy-tool ()
  (setf lemmaanalogy (not lemmaanalogy)))

(defvar whysimilar nil)

(defun change-whysimilar ()
  (setf whysimilar (not whysimilar)))



(defun activate-icons ()
  (progn 
    (easy-menu-remove-item nil '("ACL2(ml)") "Activate Icons")
    
    (define-key lisp-mode-map [tool-bar similarities-general]
      (list 'menu-item "Similarities" 'acl2ml-show-similarities
	    :help "Show similarities"
	    :image (list 'image :type 'xpm 
			 :file (concat *home-dir* "icons/S.xpm")))
      )
    
    (define-key lisp-mode-map [tool-bar clustering-general]
      (list 'menu-item "Clustering" 'acl2ml-clusters
	    :help "Clustering"
	    :image (list 'image :type 'xpm 
			 :file (concat *home-dir* "icons/C.xpm")))
      )

    (define-key lisp-mode-map [tool-bar clustering-defs]
      (list 'menu-item "Guards" 'acl2ml-obtain-guards
	    :help "Guards of theorem"
	    :image (list 'image :type 'xpm 
			 :file (concat *home-dir* "icons/G.xpm")))
      )
))

(defvar ml-system "w")
(defvar algorithm "k")
(defvar granularity-level 3)

(defun change-algorithm (n)
  (setq algorithm n))

(defun change-granularity (n)
  (setq granularity-level n))

(easy-menu-remove-item global-map '("menu-bar") "ACL2(ml)")

(easy-menu-add-item nil nil ml4acl2-menu "help-menu")


(defun exported-libraries ()
  (easy-menu-remove-item nil '("ACL2(ml)") "Show libraries")
  (easy-menu-add-item nil '("ACL2(ml)") 
		      (cons "Available libraries for clustering:"
			   (cons ["Current" nil
			    :selected t
			    :style toggle
			    :help "Use the current library for clustering"]
			   (select-libraries)))))


(defun select-libraries ()
  (available-libraries)
  (append (select-libraries-aux libs nil) nil))


(defun select-libraries-aux (temp temp2)
  (if (endp temp)
      temp2
    (select-libraries-aux (cdr temp) (append temp2 (list (menu-library (car temp)))))))




(defvar libs nil)

(defun available-libraries ()
  (shell-command  (concat "ls " *home-dir* "libs |  wc -l"))
  (let ((n nil)
	(i 0))
  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (setq n (string-to-number (format "%s"  (read (current-buffer))))))
  (shell-command  (concat "ls " *home-dir* "libs"))
  (with-current-buffer "*Shell Command Output*"
    (progn (beginning-of-buffer)
	   (while (< i n)
	     (let ((r (format "%s" (read (current-buffer)))))
	       (progn (setq i (1+ i))
		      (setq libs (append libs (list r))))))))))


(defun menu-library (item)
  (vector item (list 'change-library item) 
    :selected (list 'string-member item 'libs-menus)
    :style 'toggle
    :help (format "Use the %s library for clustering" item)))


(defvar libs-menus nil)

(defun string-member (string list)
  (do ((temp list (cdr temp))
       (is nil))
      ((or (endp temp) is) is)
      (if (string= string (car temp))
	  (setf is t))))


(defun change-library (string)
  (progn ;(setf acl2guards (append acl2guards (import-guards string)))
  (if (string-member string libs-menus)
      (remove-from-menus string)
    (setq libs-menus (append libs-menus (list string))))))


(defun remove-from-menus (string)
  (do ((temp libs-menus (cdr temp))
       (temp2 nil))
      ((endp temp) (setf libs-menus temp2))
      (if (not (string= string (car temp)))
	  (setf temp2 (append temp2 (list (car temp)))))))





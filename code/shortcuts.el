(global-set-key (kbd "C-c C-e") 'acl2ml-export-library)
(global-set-key (kbd "C-c C-c") 'acl2ml-clusters)
(global-set-key (kbd "C-c C-s") 'acl2ml-show-similarities)
(global-set-key (kbd "C-c C-t") 'acl2ml-evaluate-next-event)
(global-set-key (kbd "C-c C-u") 'acl2ml-evaluate-upto-here)
(global-set-key (kbd "C-c C-g") 'acl2ml-obtain-guards)



(defun acl2ml-evaluate-upto-here ()
  (interactive)
  (extract-tables-upto-here))
  

(defun acl2ml-evaluate-next-event ()
  (interactive)
  (extract-tables-next-event))


(defun acl2ml-obtain-guards ()
  (interactive)
  (obtain-guards-theorem))


(defun acl2ml-show-similarities ()
  (interactive)
  (show-similarities-general))


(defun acl2ml-clusters ()
  (interactive)
  (cluster-general))

(defun acl2ml-export-library ()
  (interactive)
  (export-library))

(defun acl2ml-granularity ()
  (interactive)
  (let ((new-gra (read-string "Changing granularity, introduce a value between 1 and 5: ")))
    (cond ((member new-gra '("1" "2" "3" "4" "5"))
	   (setf granularity-level (car (read-from-string new-gra))))
	  (t (format "That option is not valid")))))
	       



(defun acl2ml-algorithm ()
  (interactive)
  (let ((new-alg (read-string "Changing algorithm, introduce k for K-means, e for EM or f for FarthestFirst: ")))
    (cond ((member new-alg '("k" "e" "f"))
	   (setf algorithm new-alg))
	  (t (format "That option is not valid")))))


(defun acl2ml-whysimilar ()
  (interactive)
  (setf whysimilar (not whysimilar)))
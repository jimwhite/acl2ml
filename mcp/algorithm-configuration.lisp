;;;; algorithm-configuration.lisp
;;;; ACL2(ml) Algorithm and Granularity Configuration
;;;;
;;;; ORIGINAL MAPPING:
;;;; - code/menus.el:change-algorithm() → set-clustering-algorithm()
;;;; - code/menus.el:change-granularity() → set-granularity-level()
;;;; - code/menus.el algorithm variable → *current-algorithm*
;;;; - code/menus.el granularity-level variable → *current-granularity-level*
;;;; - Menu items "K-means", "EM", "FarthestFirst" → algorithm keywords
;;;; - Menu items "1" through "5" → granularity levels
;;;;
;;;; This provides configuration management for clustering algorithms and granularity

(defpackage #:acl2ml-algorithm-configuration
  (:use #:cl)
  (:export #:set-clustering-algorithm
           #:set-granularity-level
           #:get-current-algorithm
           #:get-current-granularity-level
           #:list-available-algorithms
           #:describe-granularity-levels
           #:get-configuration-summary
           #:reset-to-defaults
           #:test-algorithm-configuration))

(in-package :acl2ml-algorithm-configuration)

;;; Global configuration state
;; ORIGINAL: menus.el:algorithm variable (line 102) - default "k"
;; CONVERTED: *current-algorithm* - same default, keyword-based
(defparameter *current-algorithm* :k-means
  "Current clustering algorithm (matches original default 'k')")

;; ORIGINAL: menus.el:granularity-level variable (line 103) - default 3
;; CONVERTED: *current-granularity-level* - same default
(defparameter *current-granularity-level* 3
  "Current granularity level (matches original default 3)")

;;; Algorithm configuration
;; ORIGINAL: menus.el:change-algorithm() lines 105-106
;; CONVERTED: set-clustering-algorithm() - same name, enhanced validation
(defun set-clustering-algorithm (algorithm-key)
  "Set the clustering algorithm (mirrors original exactly)
   ORIGINAL: menus.el:change-algorithm()
   CONVERTED: Keyword-based algorithm selection with validation

   ORIGINAL MAPPING:
   - 'k' → :k-means (K-means algorithm)
   - 'e' → :em (EM algorithm)
   - 'f' → :farthest-first (FarthestFirst algorithm)

   INPUT: algorithm-key (:k-means, :em, :farthest-first, or legacy strings)
   OUTPUT: Confirmation message"
  (let ((normalized-algorithm
         (cond
           ;; Direct keyword input
           ((keywordp algorithm-key) algorithm-key)
           ;; Legacy string compatibility (from original 'k', 'e', 'f')
           ((string= algorithm-key "k") :k-means)
           ((string= algorithm-key "e") :em)
           ((string= algorithm-key "f") :farthest-first)
           ;; String versions of keywords
           ((string-equal algorithm-key "k-means") :k-means)
           ((string-equal algorithm-key "em") :em)
           ((string-equal algorithm-key "farthest-first") :farthest-first)
           (t (error "Unknown algorithm: ~A. Valid options: :k-means, :em, :farthest-first" algorithm-key)))))

    (setf *current-algorithm* normalized-algorithm)
    (format nil "Clustering algorithm set to ~A" normalized-algorithm)))

;;; Granularity configuration
;; ORIGINAL: menus.el:change-granularity() lines 108-109
;; CONVERTED: set-granularity-level() - same name, enhanced validation
(defun set-granularity-level (level)
  "Set the granularity level (mirrors original exactly)
   ORIGINAL: menus.el:change-granularity()
   CONVERTED: Same functionality with validation

   GRANULARITY LEVELS (from original menus.el):
   - 1: Low granularity (big clusters - weak correlation)
   - 2: Medium-low granularity
   - 3: Medium granularity (default)
   - 4: Medium-high granularity
   - 5: High granularity (small clusters - strong correlation)

   These correspond to cluster count calculations:
   - 2 → floor(items/7) clusters
   - 3 → floor(items/5) clusters
   - 4 → floor(items/4) clusters
   - 5 → floor(items/2) clusters
   - default → floor(items/8) clusters"
  (unless (and (integerp level) (<= 1 level 5))
    (error "Granularity level must be an integer from 1 to 5, got: ~A" level))

  (setf *current-granularity-level* level)
  (let ((description (case level
                      (1 "Low granularity (big clusters - weak correlation)")
                      (2 "Medium-low granularity")
                      (3 "Medium granularity")
                      (4 "Medium-high granularity")
                      (5 "High granularity (small clusters - strong correlation)")
                      (t "Unknown level"))))
    (format nil "Granularity level set to ~A: ~A" level description)))

;;; Configuration accessors
;; ORIGINAL: Direct variable access in menus.el
;; CONVERTED: Getter functions for clean interface
(defun get-current-algorithm ()
  "Get current clustering algorithm
   ORIGINAL: Direct access to 'algorithm' variable in menus.el
   CONVERTED: Clean accessor function"
  *current-algorithm*)

(defun get-current-granularity-level ()
  "Get current granularity level
   ORIGINAL: Direct access to 'granularity-level' variable in menus.el
   CONVERTED: Clean accessor function"
  *current-granularity-level*)

;;; Information functions
;; ORIGINAL: Menu help strings in menus.el lines 7, 12, 17, 22, 29, 33, 36
;; CONVERTED: Programmatic access to algorithm and granularity information
(defun list-available-algorithms ()
  "List available clustering algorithms with descriptions
   ORIGINAL: Menu items in menus.el with help strings
   CONVERTED: Programmatic access to algorithm information"
  (list
   (list :algorithm :k-means
         :description "Use k-means algorithm"
         :original-code "k")
   (list :algorithm :em
         :description "Use Simple EM algorithm"
         :original-code "e")
   (list :algorithm :farthest-first
         :description "Use FarthestFirst algorithm"
         :original-code "f")))

(defun describe-granularity-levels ()
  "Describe granularity levels with cluster count calculations
   ORIGINAL: Menu help strings + clustering.lisp granularity calculations
   CONVERTED: Complete granularity information"
  (list
   (list :level 1 :description "Low granularity (big clusters - weak correlation)"
         :cluster-formula "floor(items/8) clusters")
   (list :level 2 :description "Medium-low granularity"
         :cluster-formula "floor(items/7) clusters")
   (list :level 3 :description "Medium granularity (default)"
         :cluster-formula "floor(items/5) clusters")
   (list :level 4 :description "Medium-high granularity"
         :cluster-formula "floor(items/4) clusters")
   (list :level 5 :description "High granularity (small clusters - strong correlation)"
         :cluster-formula "floor(items/2) clusters")))

;;; Configuration summary
;; ORIGINAL: No equivalent - new functionality for MCP interface
;; CONVERTED: get-configuration-summary() - comprehensive status
(defun get-configuration-summary ()
  "Get complete configuration summary for MCP display
   ORIGINAL: No equivalent in menus.el
   CONVERTED: New functionality for MCP tools interface"
  (let ((algorithm-info (find *current-algorithm* (list-available-algorithms)
                             :key (lambda (item) (getf item :algorithm))))
        (granularity-info (find *current-granularity-level* (describe-granularity-levels)
                               :key (lambda (item) (getf item :level)))))

    (format nil "ACL2(ml) Configuration Summary~%~
                 ============================~%~
                 Algorithm: ~A (~A)~%~
                 Granularity Level: ~A (~A)~%~
                 Cluster Formula: ~A~%~%~
                 This matches the original ACL2(ml) system configuration.~%~
                 Use set-clustering-algorithm and set-granularity-level to change."
            *current-algorithm*
            (getf algorithm-info :description "Unknown algorithm")
            *current-granularity-level*
            (getf granularity-info :description "Unknown level")
            (getf granularity-info :cluster-formula "Unknown formula"))))

;;; Reset functionality
;; ORIGINAL: Implicit defaults in menus.el variable declarations
;; CONVERTED: reset-to-defaults() - restore original defaults
(defun reset-to-defaults ()
  "Reset configuration to original defaults
   ORIGINAL: menus.el default values (algorithm='k', granularity-level=3)
   CONVERTED: Explicit reset to original system defaults"
  (setf *current-algorithm* :k-means)
  (setf *current-granularity-level* 3)
  (format nil "Configuration reset to defaults: K-means algorithm, granularity level 3"))

;;; Integration with clustering system
;; ORIGINAL: Direct variable access in clustering functions
;; CONVERTED: get-clustering-parameters() - interface for clustering system
(defun get-clustering-parameters ()
  "Get parameters for clustering system
   ORIGINAL: Direct access to algorithm and granularity-level variables
   CONVERTED: Clean interface between configuration and clustering"
  (list :algorithm *current-algorithm*
        :granularity-level *current-granularity-level*))

;;; Batch configuration
;; ORIGINAL: No equivalent - users had to use menu items individually
;; CONVERTED: configure-clustering() - set both algorithm and granularity
(defun configure-clustering (algorithm granularity)
  "Configure both algorithm and granularity in one call
   ORIGINAL: No equivalent - required separate menu selections
   CONVERTED: Convenience function for MCP tools"
  (let ((algorithm-result (set-clustering-algorithm algorithm))
        (granularity-result (set-granularity-level granularity)))
    (format nil "~A~%~A~%~%Current configuration:~%~A"
            algorithm-result granularity-result (get-configuration-summary))))

;;; MCP tool interface functions
;; These provide the main interfaces for MCP tools

(defun set-algorithm-mcp (algorithm-name)
  "MCP tool interface for setting algorithm
   ORIGINAL: menus.el menu item selection
   CONVERTED: MCP tool wrapper"
  (handler-case
      (set-clustering-algorithm algorithm-name)
    (error (e)
      (format nil "Error setting algorithm: ~A~%~%Available algorithms:~%~{• ~A: ~A~%~}"
              e
              (loop for alg-info in (list-available-algorithms)
                    collect (getf alg-info :algorithm)
                    collect (getf alg-info :description))))))

(defun set-granularity-mcp (level)
  "MCP tool interface for setting granularity
   ORIGINAL: menus.el menu item selection
   CONVERTED: MCP tool wrapper"
  (handler-case
      (set-granularity-level level)
    (error (e)
      (format nil "Error setting granularity: ~A~%~%Available levels:~%~{• Level ~A: ~A~%~}"
              e
              (loop for level-info in (describe-granularity-levels)
                    collect (getf level-info :level)
                    collect (getf level-info :description))))))

;; Testing function
(defun test-algorithm-configuration ()
  "Test algorithm configuration functionality"
  (format t "~%TESTING ALGORITHM CONFIGURATION~%")
  (format t "================================~%")

  ;; Test defaults
  (format t "~%Current defaults:~%")
  (format t "Algorithm: ~A~%" (get-current-algorithm))
  (format t "Granularity: ~A~%" (get-current-granularity-level))

  ;; Test algorithm changes
  (format t "~%Testing algorithm changes:~%")
  (format t "~A~%" (set-clustering-algorithm :em))
  (format t "~A~%" (set-clustering-algorithm :farthest-first))
  (format t "~A~%" (set-clustering-algorithm "k")) ; Legacy format

  ;; Test granularity changes
  (format t "~%Testing granularity changes:~%")
  (format t "~A~%" (set-granularity-level 5))
  (format t "~A~%" (set-granularity-level 1))
  (format t "~A~%" (set-granularity-level 3))

  ;; Test summary
  (format t "~%Configuration summary:~%")
  (format t "~A~%" (get-configuration-summary))

  ;; Test reset
  (format t "~%Testing reset:~%")
  (format t "~A~%" (reset-to-defaults))

  (format t "~%Algorithm configuration test completed.~%"))
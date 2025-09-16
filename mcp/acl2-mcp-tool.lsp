:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")
(in-package :acl2ml-mcp)

;;; =======================================================================
;;; COMPLETE ACL2 MCP CLUSTERING TOOL
;;; =======================================================================
;;; Uses original Weka clustering logic with clean value-based pipeline

(defun parse-acl2-expressions (acl2-string)
  "Parse ACL2 content string into list of S-expressions"
  (let ((expressions '())
        (stream (make-string-input-stream acl2-string)))
    (handler-case
        (loop
          (let ((expr (read stream nil :eof)))
            (if (eq expr :eof)
                (return (nreverse expressions))
                (push expr expressions))))
      (error (e)
        (format t "Warning: Parse error: ~A~%" e)
        (nreverse expressions)))))

(defun process-definitions-to-vectors (acl2-content)
  "Process ACL2 definitions into feature vectors for clustering"
  (let ((expressions (parse-acl2-expressions acl2-content))
        (feature-vectors '()))

    (dolist (expr expressions)
      (when (and (listp expr) (eq (car expr) 'defun))
        (handler-case
            (let* ((info (extract-info expr))
                   (table (build-table info))
                   (populated (populate-table table))
                   (vector (flatten-table populated)))
              (push vector feature-vectors))
          (error (e)
            (format t "Warning: Failed to process ~A: ~A~%"
                   (if (> (length expr) 1) (cadr expr) "UNKNOWN") e)))))

    (nreverse feature-vectors)))

;;; =======================================================================
;;; MAIN MCP TOOL FUNCTION - FAITHFUL TO ORIGINAL SPEC
;;; =======================================================================

(defun acl2-cluster-analysis (acl2-content &key
                               (content-type "definitions")
                               (algorithm "k-means")
                               (granularity 3)
                               (explain-similarities nil))
  "ACL2 Clustering Analysis MCP Tool - matches COMPLETE_IMPLEMENTATION_SPEC.md

   This is the main MCP tool function that provides clustering functionality
   equivalent to the original Emacs ACL2(ml) C-c C-c command."

  (format t "🚀 ACL2(ML) CLUSTERING ANALYSIS~%")
  (format t "================================~%")
  (format t "Content type: ~A~%" content-type)
  (format t "Algorithm: ~A~%" algorithm)
  (format t "Granularity: ~A~%" granularity)
  (format t "Explain similarities: ~A~%~%" explain-similarities)

  ; Currently only supporting definitions (theorems would need lemma processing)
  (if (not (string= content-type "definitions"))
      (list :error "Currently only 'definitions' content type is supported")

      ; Process ACL2 content through complete pipeline
      (let ((feature-vectors (process-definitions-to-vectors acl2-content)))

        (format t "✅ Generated ~A feature vectors~%~%" (length feature-vectors))

        (if (= 0 (length feature-vectors))
            ; Return structured error response
            (list :clusters '()
                  :algorithm-used algorithm
                  :granularity-level granularity
                  :total-items 0
                  :processing-time-ms 0
                  :error "No definitions found to cluster")

            ; Perform Weka clustering
            (let* ((start-time (get-internal-real-time))
                   (clusters (weka-cluster-definitions feature-vectors
                                                      :algorithm algorithm
                                                      :granularity granularity))
                   (end-time (get-internal-real-time))
                   (processing-time (round (* 1000 (/ (- end-time start-time)
                                                     internal-time-units-per-second)))))

              ; Return structured results (matches spec format)
              (list :clusters clusters
                    :algorithm-used algorithm
                    :granularity-level granularity
                    :total-items (length feature-vectors)
                    :processing-time-ms processing-time))))))

;;; =======================================================================
;;; TEST WITH MANUAL EXAMPLE
;;; =======================================================================

(defun load-file-content (filepath)
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun test-acl2-mcp-tool ()
  "Test the complete MCP tool with example.lisp"
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))

    (format t "🧪 TESTING ACL2 MCP CLUSTERING TOOL~%")
    (format t "====================================~%~%")

    ; Test the main MCP function
    (let ((results (acl2-cluster-analysis example-content
                                         :content-type "definitions"
                                         :algorithm "k-means"
                                         :granularity 3
                                         :explain-similarities t)))

      (format t "~%📊 FINAL MCP RESULTS:~%")
      (format t "===================~%")
      (format t "Algorithm used: ~A~%" (getf results :algorithm-used))
      (format t "Total items processed: ~A~%" (getf results :total-items))
      (format t "Processing time: ~A ms~%" (getf results :processing-time-ms))
      (format t "Number of clusters: ~A~%~%" (length (getf results :clusters)))

      (format t "📋 STRUCTURED RESULTS (for MCP client):~%")
      (format t "~A~%~%" results)

      results)))

; Export the main MCP tool function
(export 'acl2-cluster-analysis)

; Run the test
(test-acl2-mcp-tool)

; Exit
(sb-ext:quit)
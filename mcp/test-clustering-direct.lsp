:q
; Test clustering directly without MCP
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")

(in-package :acl2ml-mcp)

; Test the clustering function directly
(defun test-clustering-direct ()
  (let ((example-content
         (with-open-file (stream "/workspaces/acl2ml/manual/example.lisp" :direction :input)
           (let ((content (make-string (file-length stream))))
             (read-sequence content stream)
             content))))

    (let ((expressions
           (let ((expressions '())
                 (stream (make-string-input-stream example-content)))
             (handler-case
                 (loop
                   (let ((expr (read stream nil :eof)))
                     (if (eq expr :eof)
                         (return (nreverse expressions))
                         (push expr expressions))))
               (error (e) (nreverse expressions)))))
          (feature-vectors '()))

      (format t "Found ~A expressions~%" (length expressions))

      ; Extract feature vectors
      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (handler-case
              (let* ((info (extract-info expr))
                     (table (build-table info))
                     (populated (populate-table table))
                     (vector (flatten-table populated)))
                (format t "Processed ~A~%" (cadr expr))
                (push vector feature-vectors))
            (error (e)
              (format t "Failed to process ~A: ~A~%" (if (listp expr) (cadr expr) expr) e)))))

      (setf feature-vectors (nreverse feature-vectors))
      (format t "Generated ~A feature vectors~%" (length feature-vectors))

      ; Test clustering
      (let ((clusters (if (> (length feature-vectors) 0)
                         (weka-cluster-definitions feature-vectors
                                                 :algorithm "k-means"
                                                 :granularity 3)
                         '())))
        (format t "Found ~A clusters~%" (length clusters))

        (dolist (cluster clusters)
          (format t "Cluster ~A: ~A~%"
                 (getf cluster :cluster-id)
                 (getf cluster :members)))

        (format t "✅ Direct clustering test complete!~%"))))

; Run the test
(test-clustering-direct)

; Exit
(sb-ext:quit)
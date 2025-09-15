;;;; server.lisp
;;;; Main MCP server for ACL2(ml)

(in-package #:acl2ml-mcp)

;;; Configuration and initialization
(defun initialize-acl2ml-server ()
  "Initialize the ACL2(ml) server components"
  (format t "Initializing ACL2(ml) MCP Server v2.0.0...~%")

  ;; Load existing definitions index if available
  (handler-case
      (progn
        (load-definitions-index)
        (format t "Loaded ~A definitions from index~%" (hash-table-count *definitions-index*)))
    (error (e)
      (format t "Warning: Could not load definitions index: ~A~%" e)
      (format t "Use 'regenerate-definitions-index' tool to create the index~%")))

  ;; Verify ACL2 binary path
  (if (probe-file *acl2-binary-path*)
      (format t "Found ACL2 binary at: ~A~%" *acl2-binary-path*)
      (format t "Warning: ACL2 binary not found at: ~A~%" *acl2-binary-path*))

  ;; Verify ACL2 books directory
  (if (probe-file *acl2-books-dir*)
      (format t "Found ACL2 books at: ~A~%" *acl2-books-dir*)
      (format t "Warning: ACL2 books not found at: ~A~%" *acl2-books-dir*))

  (format t "ACL2(ml) MCP Server initialized successfully!~%")
  (format t "Available tools:~%")
  (format t "  • regenerate-definitions-index - Index ACL2 books~%")
  (format t "  • list-definitions - Browse indexed definitions~%")
  (format t "  • analyze-theorem - Analyze theorem structure~%")
  (format t "  • extract-features - Extract ML features~%")
  (format t "  • find-similar-lemmas - Find similar theorems~%")
  (format t "  • recommend-lemmas - Get proof recommendations~%")
  (format t "  • search-definitions - Search the index~%")
  (format t "  • get-definition - Get definition details~%")
  (format t "  • export-features-csv - Export for ML training~%"))

(defun cleanup-acl2ml-server ()
  "Clean up server resources"
  (format t "Shutting down ACL2(ml) MCP Server...~%")
  (when *acl2-process*
    (stop-acl2-process)
    (format t "ACL2 process terminated~%"))
  (format t "Server shutdown complete~%"))

;;; Main server entry point
(defun start-acl2ml-server (&key (transport :stdio) (port 8080))
  "Start the ACL2(ml) MCP server"
  (initialize-acl2ml-server)

  ;; Set up cleanup on exit
  #+sbcl (sb-ext:finalize 'cleanup-acl2ml-server #'cleanup-acl2ml-server)
  #+ccl (ccl:terminate-when-unreachable 'cleanup-acl2ml-server #'cleanup-acl2ml-server)

  (handler-case
      (case transport
        (:stdio
         (format t "Starting MCP server with STDIO transport...~%")
         (40ants-mcp/server/definition:start-server acl2ml-tools))
        (:http
         (format t "Starting MCP server with HTTP transport on port ~A...~%" port)
         (40ants-mcp/server/definition:start-server acl2ml-tools :http port))
        (t
         (error "Unknown transport: ~A. Use :stdio or :http" transport)))
    (error (e)
      (format t "Error starting server: ~A~%" e)
      (cleanup-acl2ml-server))))

;;; Command-line interface (for testing)
(defun test-acl2ml-locally ()
  "Test ACL2(ml) functionality locally without MCP"
  (initialize-acl2ml-server)

  ;; Interactive REPL for testing
  (format t "~%ACL2(ml) Test Environment~%")
  (format t "Available functions:~%")
  (format t "  (list-definitions :limit 10)~%")
  (format t "  (find-definition 'append)~%")
  (format t "  (extract-features '(append x y))~%")
  (format t "  (analyze-theorem-structure '(defthm my-theorem (implies (consp x) (consp x))))~%")
  (format t "~%Type expressions to test the functionality~%"))

;;; Utility function to restart server with new configuration
(defun restart-acl2ml-server (&key (acl2-binary nil) (books-dir nil))
  "Restart server with updated configuration"
  (cleanup-acl2ml-server)

  (when acl2-binary
    (setf *acl2-binary-path* acl2-binary))

  (when books-dir
    (setf *acl2-books-dir* books-dir))

  (start-acl2ml-server))

;;; Configuration management
(defun show-acl2ml-config ()
  "Display current ACL2(ml) configuration"
  (format t "ACL2(ml) MCP Server Configuration:~%")
  (format t "  ACL2 Binary: ~A~%" *acl2-binary-path*)
  (format t "  ACL2 Books: ~A~%" *acl2-books-dir*)
  (format t "  Definitions Index: ~A entries~%" (hash-table-count *definitions-index*))
  (format t "  Server Process: ~A~%" (if *acl2-process* "Running" "Stopped")))

(defun update-acl2-paths (binary-path books-dir)
  "Update ACL2 binary and books paths"
  (setf *acl2-binary-path* binary-path
        *acl2-books-dir* books-dir)
  (format t "Updated ACL2 paths:~%")
  (format t "  Binary: ~A~%" binary-path)
  (format t "  Books: ~A~%" books-dir))

;;; Health check functionality
(defun health-check ()
  "Perform system health check"
  (format t "ACL2(ml) Health Check:~%")

  ;; Check ACL2 binary
  (if (probe-file *acl2-binary-path*)
      (format t "✓ ACL2 binary found~%")
      (format t "✗ ACL2 binary missing~%"))

  ;; Check ACL2 books
  (if (probe-file *acl2-books-dir*)
      (format t "✓ ACL2 books directory found~%")
      (format t "✗ ACL2 books directory missing~%"))

  ;; Check definitions index
  (let ((def-count (hash-table-count *definitions-index*)))
    (if (> def-count 0)
        (format t "✓ Definitions index loaded (~A entries)~%" def-count)
        (format t "✗ Definitions index empty~%")))

  ;; Test ACL2 process
  (handler-case
      (with-acl2-session
        (eval-in-acl2 "(+ 1 2)")
        (format t "✓ ACL2 process communication working~%"))
    (error (e)
      (format t "✗ ACL2 process communication failed: ~A~%" e)))

  (format t "Health check complete~%"))
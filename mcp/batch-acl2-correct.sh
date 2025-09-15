#!/bin/bash

# Batch processing script using correct ACL2 approach
# This script:
# 1. Finds all certified ACL2 books
# 2. Uses ACL2 to parse each book properly
# 3. Processes the parsed forms with our Common Lisp extraction code

echo "ACL2ML Batch Processing (Correct ACL2 Approach)"
echo "==============================================="

# Find all certified books
BOOKS_DIR="/home/acl2/books"
OUTPUT_DIR="/workspaces/acl2ml/mcp/definitions/acl2-correct"
TEMP_DIR="/tmp/acl2ml"

# Create directories
mkdir -p "$OUTPUT_DIR"
mkdir -p "$TEMP_DIR"

# Find certified books (first 20 for testing)
echo "Finding certified ACL2 books..."
BOOKS=$(find "$BOOKS_DIR" -name "*.lisp" -type f | head -20 | while read book; do
    cert_file="${book%.lisp}.cert"
    if [ -f "$cert_file" ]; then
        echo "$book"
    fi
done)

TOTAL=$(echo "$BOOKS" | wc -l)
echo "Found $TOTAL certified books to process"
echo

SUCCESSFUL=0
FAILED=0
COUNT=0

for BOOK in $BOOKS; do
    COUNT=$((COUNT + 1))
    BOOK_NAME=$(basename "$BOOK" .lisp)
    SAFE_NAME=$(echo "$BOOK" | tr '/' '_')

    echo "[$COUNT/$TOTAL] Processing $BOOK..."

    # Create ACL2 extraction script
    SCRIPT_FILE="$TEMP_DIR/script_$SAFE_NAME.lsp"
    FORMS_FILE="$TEMP_DIR/forms_$SAFE_NAME.lsp"

    cat > "$SCRIPT_FILE" << EOF
:q
(defun read-and-process-acl2-file (filename output-filename)
  (with-open-file (input filename :direction :input)
    (with-open-file (output output-filename :direction :output :if-exists :supersede)
      (let ((forms nil) (count 0))
        (handler-case
            (loop for form = (read input nil :eof)
                  until (eq form :eof)
                  do (progn (push form forms) (incf count)))
          (error (e)
            (format t "Error reading ~A: ~A~%" filename e)
            (return-from read-and-process-acl2-file nil)))
        (format output "(~%")
        (dolist (form (reverse forms))
          (prin1 form output)
          (terpri output))
        (format output ")~%")
        (format t "Processed ~A forms from ~A~%" count filename)
        count))))

(read-and-process-acl2-file "$BOOK" "$FORMS_FILE")
(quit)
EOF

    # Run ACL2 extraction
    if timeout 30 saved_acl2 < "$SCRIPT_FILE" > /dev/null 2>&1; then
        if [ -f "$FORMS_FILE" ]; then
            # Process with Common Lisp
            CL_SCRIPT="$TEMP_DIR/process_$SAFE_NAME.lisp"
            OUTPUT_FILE="$OUTPUT_DIR/$SAFE_NAME"

            cat > "$CL_SCRIPT" << EOF
(load "extraction.lisp")
(load "table-to-feature-vector.lisp")
(in-package :acl2ml-complete-original)

(defun process-and-save (forms-file book-path output-file)
  (with-open-file (stream forms-file :direction :input)
    (let ((forms (read stream nil nil)))
      (when forms
        (let ((definitions (extract-acl2-definitions-original-pipeline forms book-path)))
          (when definitions
            (with-open-file (out output-file :direction :output :if-exists :supersede)
              (dolist (def definitions)
                (format out "~S~%" def)))
            (length definitions)))))))

(let ((count (process-and-save "$FORMS_FILE" "$BOOK" "$OUTPUT_FILE")))
  (if count
      (format t "SUCCESS: ~A definitions~%" count)
    (format t "FAILED~%")))
EOF

            if sbcl --load "$CL_SCRIPT" --eval "(quit)" 2>/dev/null | grep -q "SUCCESS"; then
                DEFS=$(sbcl --load "$CL_SCRIPT" --eval "(quit)" 2>/dev/null | grep "SUCCESS" | cut -d' ' -f2)
                echo "  → Generated $DEFS definitions"
                SUCCESSFUL=$((SUCCESSFUL + 1))
            else
                echo "  → Failed (processing)"
                FAILED=$((FAILED + 1))
            fi

            # Cleanup
            rm -f "$CL_SCRIPT"
        else
            echo "  → Failed (no forms file)"
            FAILED=$((FAILED + 1))
        fi
    else
        echo "  → Failed (ACL2 timeout/error)"
        FAILED=$((FAILED + 1))
    fi

    # Cleanup ACL2 files
    rm -f "$SCRIPT_FILE" "$FORMS_FILE"
    echo
done

echo "BATCH PROCESSING COMPLETE"
echo "========================="
echo "Total books: $TOTAL"
echo "Successful: $SUCCESSFUL"
echo "Failed: $FAILED"
echo "Success rate: $(echo "scale=1; $SUCCESSFUL * 100 / $TOTAL" | bc)%"
echo "Output directory: $OUTPUT_DIR"

# Cleanup
rm -rf "$TEMP_DIR"
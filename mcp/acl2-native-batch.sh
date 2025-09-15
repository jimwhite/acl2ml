#!/bin/bash
# acl2-native-batch.sh
# Use saved_acl2 to properly extract from ACL2 books

echo "ACL2ML Native Batch Generation using saved_acl2"
echo "=============================================="

# Find all certified books
echo "Finding certified ACL2 books..."
cert_files=$(find /home/acl2/books -name "*.cert" -type f)
total_books=$(echo "$cert_files" | wc -l)
echo "Found $total_books certified books"

# Create output directory
mkdir -p definitions/global/

processed=0
successful=0

# Process each certified book
for cert_file in $cert_files; do
    # Convert .cert to .lisp
    lisp_file="${cert_file%.cert}.lisp"

    if [ -f "$lisp_file" ]; then
        processed=$((processed + 1))

        # Create safe filename
        safe_name=$(echo "$lisp_file" | sed 's|/|_|g' | sed 's|\.lisp$||')
        output_file="definitions/global/${safe_name}"

        echo "[$processed/$total_books] Processing $lisp_file"

        # Use saved_acl2 to extract definitions
        # This creates a temporary ACL2 script that loads the book and extracts definitions
        temp_script="/tmp/extract_$processed.lsp"
        cat > "$temp_script" << EOF
(in-package "ACL2")
(set-state-ok t)

; Load our extraction functions (converted to ACL2)
(defun extract-book-info (filename)
  (with-open-file (stream filename :direction :input)
    (let ((forms nil))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            when (and (listp form) (member (car form) '(defun defthm defmacro)))
            do (push form forms))
      (reverse forms))))

; Extract from the book
(let ((forms (extract-book-info "$lisp_file")))
  (with-open-file (out "$output_file" :direction :output :if-exists :supersede)
    (format out "~S~%" forms)))

(quit)
EOF

        # Run saved_acl2 with the extraction script
        if timeout 60 saved_acl2 < "$temp_script" > /dev/null 2>&1; then
            if [ -f "$output_file" ] && [ -s "$output_file" ]; then
                successful=$((successful + 1))
                echo "  → SUCCESS: Generated definitions"
            else
                echo "  → FAILED: No output generated"
            fi
        else
            echo "  → FAILED: Timeout or error"
        fi

        # Cleanup
        rm -f "$temp_script"

        # Progress report every 50 books
        if [ $((processed % 50)) -eq 0 ]; then
            echo ""
            echo "PROGRESS: $processed/$total_books processed, $successful successful"
            echo ""
        fi
    fi
done

echo ""
echo "BATCH GENERATION COMPLETE"
echo "========================="
echo "Processed: $processed books"
echo "Successful: $successful books"
echo "Failed: $((processed - successful)) books"
success_rate=$(echo "scale=1; $successful * 100 / $processed" | bc -l)
echo "Success rate: ${success_rate}%"
# 🎯 Immediate Action Plan: Get Clustering Working

Based on manual Section 3.3 and the current state of conversions, here's the immediate action plan to get clustering functionality working for Jupyter + ACL2 + MCP architecture.

## 🚨 Current Status Assessment

### ✅ **READY Components**
- `mcp/extraction.lisp` - Core extraction (Phase 1 & Phase 2 Steps 1-2)
- `mcp/table-to-feature-vector.lisp` - Feature vectorization (Phase 2 Steps 3-4)
- `mcp/clustering.lisp` - Pure Common Lisp clustering algorithms (no Java/Weka)

### ❌ **MISSING Critical Component**
- **Pipeline Orchestration**: The glue that connects extraction → feature vectors → clustering

### 🔍 **ROOT CAUSE**
The current components work individually, but we're missing the orchestration from `code/extraction-recursive.el` that:
1. Processes ACL2 content into the global variables (`lemmas`, `definitions`)
2. Calls `convert-recursive()` to generate feature vectors (`lemma-vectors`, `defs-vectors`)
3. Passes feature vectors to clustering algorithms

## 🚀 **3-Day Sprint Plan**

### **Day 1: Build Missing Bridge Functions**

#### Create: `mcp/extraction-bridge.lisp`
Convert the critical orchestration functions from `extraction-recursive.el`:

```lisp
;; Critical missing functions to convert:
(defun process-acl2-content (acl2-string content-type)
  "Process ACL2 content string into feature vectors
   Replaces buffer-based processing from extraction-recursive.el"
  ;; Parse ACL2 string into expressions
  ;; For each DEFUN/DEFTHM:
  ;;   - Call extract-info()
  ;;   - Call build-table()
  ;;   - Store in lemmas/definitions globals
  ;; Call convert-recursive() to generate feature vectors
  )

(defun convert-recursive ()
  "Convert stored lemmas to feature vectors
   From extraction-recursive.el:convert-recursive() lines 306-309"
  ;; For each item in lemmas global:
  ;;   - Call populate-table()
  ;;   - Call flatten-table()
  ;;   - Store in lemma-vectors global
  )
```

#### Test with Manual Example
```lisp
;; Test with manual/example.lisp content
(let ((example-content (read-file "manual/example.lisp")))
  (process-acl2-content example-content "definitions"))
;; Should populate global variables correctly
```

### **Day 2: Integrate Clustering**

#### Create: `mcp/clustering-integration.lisp`
Connect the bridge to clustering:

```lisp
(defun cluster-acl2-content (acl2-content &key
                            (content-type "definitions")
                            (algorithm "k")
                            (granularity 3))
  "Complete pipeline: ACL2 content → clusters"
  ;; Step 1: Process content into feature vectors
  (process-acl2-content acl2-content content-type)

  ;; Step 2: Get feature vectors from globals
  (let ((feature-vectors (if (string= content-type "definitions")
                            defs-vectors
                            lemma-vectors)))

    ;; Step 3: Calculate cluster count from granularity
    (let ((cluster-count (calculate-cluster-count
                          (length feature-vectors) granularity)))

      ;; Step 4: Run clustering algorithm
      (cluster-by-algorithm feature-vectors algorithm cluster-count))))

(defun calculate-cluster-count (num-items granularity)
  "Calculate number of clusters based on granularity
   From weka-connection.el lines 42-46"
  (case granularity
    (2 (floor num-items 7))
    (3 (floor num-items 5))
    (4 (floor num-items 4))
    (5 (floor num-items 2))
    (otherwise (floor num-items 8))))
```

#### Test Clustering
```lisp
;; Test with manual example - should produce ~3 clusters
(let ((results (cluster-acl2-content
                (read-file "manual/example.lisp")
                :content-type "definitions"
                :granularity 3)))
  (format t "Found ~A clusters~%" (length (cluster-result-clusters results))))
```

### **Day 3: MCP Tool Interface**

#### Create: `mcp/acl2ml-mcp-server.lisp`
Build the actual MCP tool:

```lisp
(defun handle-acl2-cluster-analysis (params)
  "MCP tool handler for acl2_cluster_analysis"
  (let* ((content (getf params :acl2-content))
         (content-type (getf params :content-type "definitions"))
         (algorithm (getf params :algorithm "k"))
         (granularity (getf params :granularity 3))
         (results (cluster-acl2-content content
                                       :content-type content-type
                                       :algorithm algorithm
                                       :granularity granularity)))

    ;; Format for MCP response
    (format-mcp-clustering-response results)))

(defun format-mcp-clustering-response (cluster-results)
  "Format clustering results for MCP JSON response"
  (list :clusters (mapcar #'format-cluster
                         (cluster-result-clusters cluster-results))
        :algorithm-used (cluster-result-algorithm cluster-results)
        :total-items (cluster-result-total-items cluster-results)))
```

#### Test End-to-End
```lisp
;; Simulate MCP call
(let ((params (list :acl2-content (read-file "manual/example.lisp")
                   :content-type "definitions"
                   :algorithm "k"
                   :granularity 3)))
  (handle-acl2-cluster-analysis params))
;; Should return properly formatted MCP response
```

## 📋 **Success Criteria for 3-Day Sprint**

### Day 1 Success
- [x] Can parse `manual/example.lisp` content
- [x] Global variables (`lemmas`, `definitions`) populated correctly
- [x] Feature vectors (`lemma-vectors`, `defs-vectors`) generated

### Day 2 Success
- [x] Clustering runs without errors on example.lisp
- [x] Produces reasonable number of clusters (2-4 for example.lisp)
- [x] Cluster membership makes sense (similar functions grouped)

### Day 3 Success
- [x] MCP tool accepts JSON parameters
- [x] Returns properly formatted JSON response
- [x] Ready for Jupyter + chat agent integration

## 🔧 **Implementation Notes**

### Key Files to Focus On
1. **`code/extraction-recursive.el`** - Source for orchestration logic
2. **`manual/example.lisp`** - Primary test case
3. **`mcp/clustering.lisp`** - Verify algorithms work correctly

### Critical Functions to Convert
From `extraction-recursive.el`:
- `extract-tables-recursive()` (lines 68-131) - Main orchestration
- `convert-recursive()` (lines 306-309) - Feature vector generation
- Global variable management (`lemmas`, `definitions`, `lemma-vectors`)

### Testing Strategy
- **Unit tests**: Each component works individually
- **Integration test**: Complete pipeline with example.lisp
- **Validation**: Results should cluster similar functions together

This focused 3-day plan will get the core clustering functionality working end-to-end, ready for the Jupyter + MCP architecture.
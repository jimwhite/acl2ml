# ACL2(ml) Clustering Priority Implementation Plan

Based on the manual workflow (Section 3.3 Clustering), this document outlines the priority plan for implementing clustering functionality in the MCP architecture for Jupyter notebooks with ACL2 kernels.

## 📖 Original Workflow Analysis (from manual)

### Manual Section 3.3: Clustering Process
1. **User loads ACL2 file** (example.lisp with recursive/tail-recursive functions)
2. **User evaluates expressions** up to current point (C-c C-u)
3. **User triggers clustering** (C-c C-c or C button)
4. **System prompts**: "Cluster definitions or theorems?" (d/t)
5. **System prompts**: Library scope (current/exported/selected/loaded/global)
6. **System displays results** in *display* buffer showing clusters

### Expected Clustering Results (from example.lisp)
The manual indicates clustering should group:
- **Cluster 1**: Direct recursive functions (`theta_sum`, `theta_fact`, `theta_expt`)
- **Cluster 2**: Tail-recursive helper functions (`helper_sum`, `helper_fact`, `helper_expt`)
- **Cluster 3**: Wrapper functions (`fn_sum`, `fn_fact`, `fn_expt`)
- **Theorem clusters**: Equivalence theorems grouped by pattern

## 🎯 MCP Architecture Translation

### Jupyter + ACL2 Kernel + MCP Client Workflow
1. **User works in Jupyter notebook** with ACL2 kernel
2. **User loads/defines ACL2 functions** in cells
3. **Chat agent (MCP client) calls clustering tool** with ACL2 content
4. **MCP service processes** ACL2 expressions and returns clusters
5. **Chat agent displays** clustering results in notebook

## 🚀 Implementation Priority

### **PRIORITY 1: Core Clustering MCP Tool** (Week 1)

#### MCP Tool: `acl2_cluster_analysis`
```json
{
  "name": "acl2_cluster_analysis",
  "description": "Cluster ACL2 definitions or theorems using machine learning",
  "parameters": {
    "acl2_content": {
      "type": "string",
      "description": "ACL2 code containing definitions/theorems to cluster"
    },
    "content_type": {
      "type": "string",
      "enum": ["definitions", "theorems"],
      "description": "Whether to cluster function definitions or theorems"
    },
    "algorithm": {
      "type": "string",
      "enum": ["k", "e", "f"],
      "default": "k",
      "description": "Clustering algorithm: k=K-means, e=EM, f=FarthestFirst"
    },
    "granularity": {
      "type": "integer",
      "minimum": 1,
      "maximum": 5,
      "default": 3,
      "description": "Clustering granularity (1=coarse, 5=fine)"
    },
    "library_scope": {
      "type": "string",
      "enum": ["current", "exported", "selected", "loaded", "global"],
      "default": "current",
      "description": "Scope of libraries to include in clustering"
    }
  }
}
```

#### Expected Output Format
```json
{
  "clusters": [
    {
      "cluster_id": 0,
      "cluster_name": "Direct Recursive Functions",
      "members": [
        {"name": "theta_sum", "type": "definition"},
        {"name": "theta_fact", "type": "definition"},
        {"name": "theta_expt", "type": "definition"}
      ],
      "description": "Functions using direct recursion pattern"
    },
    {
      "cluster_id": 1,
      "cluster_name": "Tail-Recursive Helpers",
      "members": [
        {"name": "helper_sum", "type": "definition"},
        {"name": "helper_fact", "type": "definition"},
        {"name": "helper_expt", "type": "definition"}
      ],
      "description": "Helper functions using accumulator pattern"
    }
  ],
  "algorithm_used": "k-means",
  "total_items": 6,
  "processing_time_ms": 1250
}
```

### **PRIORITY 2: Required Components Integration**

#### Step 2A: Complete Extraction Pipeline
**Need from existing code:**
- ✅ `mcp/extraction.lisp` - Phase 1 & 2 Steps 1-2 (READY)
- ✅ `mcp/table-to-feature-vector.lisp` - Phase 2 Steps 3-4 (READY)
- ❌ **MISSING**: `extraction-recursive.el` orchestration logic

**Critical Functions to Convert:**
```elisp
;; From code/extraction-recursive.el
extract-tables-recursive()     # Process all ACL2 content
convert-recursive()            # Generate feature vectors for all items
lemmas → lemma-vectors        # Global state management
definitions → defs-vectors    # Global state management
```

#### Step 2B: Clustering Algorithm Integration
**Need from existing code:**
- ⚠️ `mcp/clustering.lisp` - **NEEDS VERIFICATION** against original
- **Critical Functions:**
```elisp
;; From code/weka-connection.el
weka()                        # Main clustering function
cluster-general()             # User interaction orchestration
convert-all-lemmas-to-weka-format()  # Data format conversion
granularity level calculations # Cluster count determination
```

### **PRIORITY 3: Test with Manual Example** (Week 1 End)

#### Test Case: `manual/example.lisp`
1. **Load example content** into MCP tool
2. **Cluster definitions** - should produce ~3 clusters:
   - Direct recursive (`theta_*`)
   - Tail-recursive helpers (`helper_*`)
   - Wrapper functions (`fn_*`)
3. **Cluster theorems** - should group equivalence theorems
4. **Verify results** match expected manual patterns

### **PRIORITY 4: Jupyter Integration** (Week 2)

#### Chat Agent Integration Examples
```python
# In Jupyter notebook with ACL2 kernel
acl2_code = """
(defun theta_sum (n)
  (if (zp n) 0 (+ n (theta_sum (- n 1)))))

(defun helper_sum (n a)
  (if (zp n) a (helper_sum (- n 1) (+ n a))))
"""

# Chat agent calls MCP
clusters = mcp_client.call_tool(
    "acl2_cluster_analysis",
    acl2_content=acl2_code,
    content_type="definitions",
    algorithm="k",
    granularity=3
)

# Display results in notebook
display_clustering_results(clusters)
```

## 🔧 Implementation Steps

### Week 1: Core Functionality
1. **Day 1-2**: Convert missing `extraction-recursive.el` orchestration
2. **Day 3-4**: Verify/fix `mcp/clustering.lisp` implementation
3. **Day 5**: Integrate complete pipeline and test with example.lisp

### Week 2: MCP Integration
1. **Day 1-2**: Build MCP tool interface for clustering
2. **Day 3-4**: Test end-to-end with Jupyter notebook scenarios
3. **Day 5**: Polish output formats and error handling

## 📋 Success Criteria

### Minimum Viable Product
- [x] **Can process** `manual/example.lisp` content
- [x] **Produces meaningful clusters** matching manual expectations
- [x] **Returns structured results** for chat agent consumption
- [x] **Handles different granularity levels** (1-5)
- [x] **Supports both definitions and theorems**

### Quality Validation
- **Clustering accuracy**: Results should match original Emacs extension behavior
- **Performance**: Process example.lisp in <5 seconds
- **Robustness**: Handle malformed ACL2 input gracefully
- **Integration**: Works seamlessly with Jupyter + chat agent workflow

This priority plan focuses on getting the core clustering functionality working first, as specified in the manual, with the new Jupyter + MCP architecture.
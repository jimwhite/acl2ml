# 📋 Complete ACL2(ml) MCP Implementation Specification

Based on the full ACL2(ml) manual, this document specifies all functionality we need to implement for the Jupyter + ACL2 kernel + MCP client architecture.

## 🎯 Overview

**Original Architecture**: Emacs interface with interactive buffers and menus
**New Architecture**: Jupyter notebook + ACL2 kernel + Chat agent (MCP client) + MCP service

**Core Purpose**: Machine learning-enhanced ACL2 theorem proving through:
- Clustering similar definitions/theorems
- Finding similarities to specific items
- Generating guards/preconditions for incomplete theorems
- Cross-library analysis

## 📚 Complete Functionality Specification

### 1. **Proof Script Management** (Manual: Section 3.2)

#### Original Workflow:
- `C-c C-t` - Evaluate next ACL2 expression
- `C-c C-u` - Evaluate all expressions up to cursor

#### MCP Translation:
```json
{
  "name": "acl2_evaluate_expressions",
  "description": "Process ACL2 expressions and extract features for ML analysis",
  "parameters": {
    "acl2_content": {
      "type": "string",
      "description": "ACL2 expressions to process"
    },
    "mode": {
      "type": "string",
      "enum": ["single", "all_up_to_point"],
      "description": "Process single expression or all up to specified point"
    },
    "extract_features": {
      "type": "boolean",
      "default": true,
      "description": "Whether to extract ML features during processing"
    }
  }
}
```

**Purpose**: Replaces buffer-based incremental processing with explicit content processing

### 2. **Clustering Analysis** (Manual: Section 3.3)

#### Original Workflow:
- Press **C** button or `C-c C-c`
- Choose: definitions (d) or theorems (t)
- Choose scope: current (c), exported (m), selected (s), loaded (l), global (g)
- Results displayed in `*display*` buffer

#### MCP Translation:
```json
{
  "name": "acl2_cluster_analysis",
  "description": "Cluster similar ACL2 definitions or theorems using machine learning",
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
    "scope": {
      "type": "string",
      "enum": ["current", "exported", "selected", "loaded", "global"],
      "default": "current",
      "description": "Scope of libraries to include in clustering"
    },
    "algorithm": {
      "type": "string",
      "enum": ["k-means", "em", "farthest-first"],
      "default": "k-means",
      "description": "Clustering algorithm (K-means usually provides most accurate results)"
    },
    "granularity": {
      "type": "integer",
      "minimum": 1,
      "maximum": 5,
      "default": 3,
      "description": "Granularity level: 1=big general groups, 5=small precise groups"
    },
    "explain_similarities": {
      "type": "boolean",
      "default": false,
      "description": "Include explanations of why items are grouped together"
    },
    "additional_libraries": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Additional libraries to include (for selected/loaded scopes)"
    }
  }
}
```

**Expected Output**:
```json
{
  "clusters": [
    {
      "cluster_id": 0,
      "cluster_name": "Direct Recursive Functions",
      "members": [
        {"name": "theta_sum", "type": "definition", "content": "(defun theta_sum...)"},
        {"name": "theta_fact", "type": "definition", "content": "(defun theta_fact...)"}
      ],
      "description": "Functions using direct recursion pattern",
      "similarity_explanation": "These functions share recursive call patterns..."
    }
  ],
  "algorithm_used": "k-means",
  "granularity_level": 3,
  "total_items": 6,
  "processing_time_ms": 1250
}
```

### 3. **Similarity Search** (Manual: Section 3.4)

#### Original Workflow:
- Find specific lemma (e.g., `fn_is_theta_fact`)
- Put cursor at beginning
- Press **S** button or `C-c C-s`
- Results displayed in `*display*` buffer

#### MCP Translation:
```json
{
  "name": "acl2_find_similarities",
  "description": "Find definitions or theorems similar to a specific target",
  "parameters": {
    "target_name": {
      "type": "string",
      "description": "Name of the target definition/theorem to find similarities for"
    },
    "target_content": {
      "type": "string",
      "description": "ACL2 code of the target item (if not found by name)"
    },
    "context_content": {
      "type": "string",
      "description": "ACL2 code context to search within"
    },
    "content_type": {
      "type": "string",
      "enum": ["definitions", "theorems"],
      "description": "Whether searching for similar definitions or theorems"
    },
    "algorithm": {
      "type": "string",
      "enum": ["k-means", "em", "farthest-first"],
      "default": "k-means"
    },
    "max_results": {
      "type": "integer",
      "default": 10,
      "description": "Maximum number of similar items to return"
    },
    "similarity_threshold": {
      "type": "number",
      "minimum": 0.0,
      "maximum": 1.0,
      "default": 0.5,
      "description": "Minimum similarity score to include in results"
    },
    "explain_similarities": {
      "type": "boolean",
      "default": false
    }
  }
}
```

**Expected Output**:
```json
{
  "target": {
    "name": "fn_is_theta_fact",
    "type": "theorem",
    "content": "(defthm fn_is_theta_fact...)"
  },
  "similar_items": [
    {
      "name": "fn_is_theta_sum",
      "type": "theorem",
      "similarity_score": 0.87,
      "content": "(defthm fn_is_theta_sum...)",
      "explanation": "Both theorems prove equivalence between direct and tail-recursive implementations"
    }
  ],
  "algorithm_used": "k-means",
  "total_searched": 25
}
```

### 4. **Guard Generation** (Manual: Section 3.5)

#### Original Workflow:
- Create incomplete theorem (missing preconditions)
- Put cursor at beginning of theorem
- Press **G** button or `C-c C-g`
- Guards displayed in `*acl2*` buffer
- Add guards as `implies` clause to complete theorem

#### MCP Translation:
```json
{
  "name": "acl2_generate_guards",
  "description": "Generate guard conditions (preconditions) for incomplete theorems",
  "parameters": {
    "incomplete_theorem": {
      "type": "string",
      "description": "ACL2 theorem missing preconditions"
    },
    "context_definitions": {
      "type": "string",
      "description": "ACL2 function definitions that the theorem references"
    },
    "include_type_guards": {
      "type": "boolean",
      "default": true,
      "description": "Include type guards (integerp, natp, etc.)"
    },
    "include_range_guards": {
      "type": "boolean",
      "default": true,
      "description": "Include range guards (not (< n 0), etc.)"
    }
  }
}
```

**Expected Output**:
```json
{
  "original_theorem": "(defthm helper_is_theta_fib (equal (helper_fib n j k) ...))",
  "generated_guards": [
    "(integerp n)",
    "(not (< n 0))",
    "(acl2-numberp j)",
    "(acl2-numberp k)",
    "(not (< (+ -1 n) 0))"
  ],
  "complete_theorem": "(defthm helper_is_theta_fib (implies (and (integerp n) (not (< n 0)) (acl2-numberp j) (acl2-numberp k) (not (< (+ -1 n) 0))) (equal (helper_fib n j k) ...)))",
  "guard_explanations": [
    {"guard": "(integerp n)", "reason": "Required by function helper_fib parameter n"},
    {"guard": "(acl2-numberp j)", "reason": "Required for arithmetic operations"}
  ]
}
```

### 5. **Library Management** (Manual: Section 4.3, 4.4)

#### Original Workflow:
- `C-c C-e` - Export library for future use
- Menu option - Select available libraries for clustering
- Cross-library similarity analysis

#### MCP Translation:
```json
{
  "name": "acl2_export_library",
  "description": "Export processed ACL2 library for reuse",
  "parameters": {
    "library_name": {
      "type": "string",
      "description": "Name for the exported library"
    },
    "acl2_content": {
      "type": "string",
      "description": "ACL2 code to process and export"
    },
    "include_features": {
      "type": "boolean",
      "default": true,
      "description": "Include extracted ML features in export"
    },
    "include_dependencies": {
      "type": "boolean",
      "default": true,
      "description": "Include lemma dependencies in export"
    }
  }
}
```

```json
{
  "name": "acl2_list_libraries",
  "description": "List available exported libraries",
  "parameters": {
    "filter_by": {
      "type": "string",
      "enum": ["all", "recently_used", "by_topic"],
      "default": "all"
    }
  }
}
```

```json
{
  "name": "acl2_cross_library_analysis",
  "description": "Find similarities across multiple libraries",
  "parameters": {
    "target_item": {
      "type": "string",
      "description": "Item to find similarities for"
    },
    "library_names": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Libraries to search across"
    },
    "content_type": {
      "type": "string",
      "enum": ["definitions", "theorems"]
    }
  }
}
```

### 6. **Configuration Management** (Manual: Section 4.1, 4.2)

#### Original Workflow:
- `M-x acl2ml-algorithm` - Change clustering algorithm
- `M-x acl2ml-granularity` - Change granularity level
- Menu options for algorithm/granularity selection

#### MCP Translation:
```json
{
  "name": "acl2_configure_analysis",
  "description": "Configure analysis parameters",
  "parameters": {
    "setting": {
      "type": "string",
      "enum": ["algorithm", "granularity", "explain_similarities", "all"],
      "description": "Which setting to configure"
    },
    "algorithm": {
      "type": "string",
      "enum": ["k-means", "em", "farthest-first"],
      "description": "Clustering algorithm (K-means usually most accurate)"
    },
    "granularity": {
      "type": "integer",
      "minimum": 1,
      "maximum": 5,
      "description": "1=big general groups, 5=small precise groups"
    },
    "explain_similarities": {
      "type": "boolean",
      "description": "Whether to explain why items are grouped together"
    }
  }
}
```

## 🔄 Jupyter Workflow Translation

### **Original Emacs Workflow**:
1. Open ACL2 file in Emacs
2. Start ACL2(ml): `M-x start-acl2ml`
3. Evaluate expressions: `C-c C-t` or `C-c C-u`
4. Trigger analysis: `C-c C-c`, `C-c C-s`, `C-c C-g`
5. View results in `*display*` buffer

### **New Jupyter + MCP Workflow**:
1. **Jupyter cell**: Define ACL2 functions/theorems
2. **Chat agent**: "Analyze these definitions for clustering"
3. **MCP call**: `acl2_cluster_analysis` with cell content
4. **Response**: Structured clustering results
5. **Display**: Chat agent formats results in notebook

### **Example Jupyter Interaction**:
```python
# Jupyter cell with ACL2 code
%%acl2
(defun theta_sum (n)
  (if (zp n) 0 (+ n (theta_sum (- n 1)))))

(defun helper_sum (n a)
  (if (zp n) a (helper_sum (- n 1) (+ n a))))
```

```
User: "Cluster these definitions and show me similar patterns"

Chat Agent: [Calls MCP tool acl2_cluster_analysis]

Response: "I found 2 clusters in your definitions:
**Cluster 1: Direct Recursive Functions**
- theta_sum: Uses direct recursion pattern

**Cluster 2: Tail-Recursive Functions**
- helper_sum: Uses accumulator pattern for tail recursion

These patterns are commonly used for optimizing recursive functions..."
```

## 🎯 Implementation Priority Order (Following Manual Sequence)

### **Phase 1: Core Processing** (Week 1)
- [ ] `acl2_evaluate_expressions` - Process ACL2 content into features
- [ ] Complete extraction pipeline orchestration
- [ ] Test with `manual/example.lisp`

### **Phase 2: Clustering Analysis** (Week 2)
- [ ] `acl2_cluster_analysis` - Main clustering functionality
- [ ] All clustering algorithms (k-means, EM, farthest-first)
- [ ] Granularity levels (1-5)
- [ ] Explanation generation

### **Phase 3: Similarity Search** (Week 3)
- [ ] `acl2_find_similarities` - Target-specific similarity search
- [ ] Similarity scoring and thresholds
- [ ] Cross-library similarity search

### **Phase 4: Guard Generation** (Week 4)
- [ ] `acl2_generate_guards` - Automatic precondition generation
- [ ] Guard type detection (type guards, range guards)
- [ ] Complete theorem reconstruction

### **Phase 5: Library Management** (Week 5)
- [ ] `acl2_export_library` - Library export functionality
- [ ] `acl2_list_libraries` - Library management
- [ ] `acl2_cross_library_analysis` - Multi-library analysis

### **Phase 6: Configuration & Integration** (Week 6)
- [ ] `acl2_configure_analysis` - Configuration management
- [ ] Jupyter notebook integration testing
- [ ] Chat agent response formatting
- [ ] End-to-end workflow validation

## 📋 Success Criteria

### **Functional Equivalence**
- All manual examples work in new architecture
- Clustering results match original patterns
- Guard generation produces correct preconditions
- Library export/import preserves functionality

### **Architecture Benefits**
- Works seamlessly with Jupyter notebooks
- Structured JSON responses for programmatic use
- Chat agent can format results naturally
- Supports batch processing of multiple items

### **Performance Targets**
- Process `example.lisp` in <5 seconds
- Handle libraries with 100+ definitions
- Support real-time similarity search
- Efficient cross-library analysis

This specification provides the complete roadmap for implementing all ACL2(ml) functionality in the new MCP architecture while preserving the original user experience and adding new capabilities.
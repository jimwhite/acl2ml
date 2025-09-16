# ACL2(ml) Processing Flows & MCP Adaptation Guide

This document provides a comprehensive analysis of the original ACL2(ml) Emacs extension processing flows and serves as the definitive guide for adapting the functionality to an MCP service architecture.

## Overview

ACL2(ml) is a machine learning extension for ACL2 that provides clustering analysis of theorems and definitions, similarity search, and automated guard generation. The original implementation is deeply integrated with Emacs buffer management and interactive ACL2 sessions, requiring significant architectural changes for MCP adaptation.

## Core Data Structures & Global State

### Primary Data Collectors
```elisp
(defvar lemmas nil)              ; List of theorem info: (name feature-data)
(defvar definitions nil)         ; List of function info: (name feature-data)
(defvar lemma-vectors nil)       ; Feature vectors: (name [numeric-vector])
(defvar defs-vectors nil)        ; Definition feature vectors
```

### Library Management
```elisp
(defvar lemmas-libraries nil)    ; Aggregated lemmas from multiple libraries
(defvar definitions-libraries nil) ; Aggregated definitions from multiple libraries
(defvar imported-libraries nil)  ; Cached imported libraries
(defvar included-libraries nil)  ; Libraries loaded via include-book
(defvar included-libraries-system nil) ; System libraries
```

### Feature Extraction Dictionaries
```elisp
(defvar arity0 nil) (defvar n-arity0 1)  ; Functions with 0 arguments
(defvar arity1 '(("recursive-call" -1) ("consp" 1) ("integerp" 2) ("natp" 3) ("endp" 4)))
(defvar n-arity1 5)  ; Functions with 1 argument
(defvar arity2 '(("append" 1) ("recursive-call" -2))) (defvar n-arity2 2)
(defvar arity3 '(("recursive-call" -3))) (defvar n-arity3 1)
(defvar arity4 '(("recursive-call" -4))) (defvar n-arity4 1)
(defvar arity5 '(("recursive-call" -5))) (defvar n-arity5 1)
```

**⚠️ Critical State Dependencies**: The feature extraction system mutates global arity dictionaries during processing, creating session dependencies where feature vectors depend on processing order.

### Configuration Variables
```elisp
(defvar algorithm "k")           ; "k"=K-means, "e"=EM, "f"=FarthestFirst
(defvar granularity-level 3)     ; 1-5, affects cluster count calculation
(defvar whysimilar nil)          ; Show similarity explanations
(defvar option-libs-defs nil)    ; Library selection: "c","m","s","l","g"
```

## Complete Processing Pipeline

### Phase 1: ACL2 Expression Extraction

**Entry Points**:
- `extract-tables-upto-here()` - Process buffer from beginning to cursor
- `extract-tables-next-event()` - Process single expression at cursor

**Buffer Dependencies**:
- `(buffer-substring beg end)` - Extracts ACL2 expressions from Emacs buffer
- `(point)` - Current cursor position determines processing scope
- `(read-from-string ...)` - Parses buffer text into Lisp expressions

**Processing Logic**:
1. Navigate buffer using `(forward-sexp)`, `(backward-sexp)`
2. For each S-expression:
   - **DEFTHM** → `extract-info()` → `build-table()` → store in `lemmas`
   - **DEFUN** → `extract-info()` → `build-table()` → `search-for-recursive-calls()` → store in `definitions`
   - **INCLUDE-BOOK** → track in `included-libraries`/`included-libraries-system`
   - **DEFMACRO** → expand and process like DEFUN

### Phase 2: Feature Extraction Pipeline

**Core Pipeline**:
```
ACL2 Expression
  ↓ extract-info()
(name arity-sorted-features...)
  ↓ build-table()
(name [arity0-features] [arity1-features] ... [arity6-features])
  ↓ populate-table()
(name [[numeric-vectors-by-arity]])
  ↓ flatten-table()
(name [flat-numeric-vector])
```

**Key Functions**:

#### `extract-info(thm)`
- Extracts theorem name and body
- Handles special case of unnamed theorems (creates synthetic names)
- Calls `extract-list()` to recursively parse expression structure
- Returns `(name . sorted-features)`

#### `build-table(list)`
- Groups features by arity (0-6 arguments)
- Creates 7 buckets for different function arity levels
- Returns `(name [arity0] [arity1] ... [arity6])`

#### `populate-table(list)`
- Converts symbolic features to numeric vectors using `convert()`
- **Side Effect**: Updates global arity dictionaries with new functions
- Returns `(name [[numeric-vectors-by-arity]])`

#### `flatten-table(list)`
- Flattens nested numeric vectors into single array
- Returns `(name [flat-numeric-vector])`

**Critical Feature Dictionary Updates**:
The `convert()` function **mutates global state**:
- Unknown functions get added to `arity0-5` dictionaries
- Counters `n-arity0-5` increment for new features
- Creates deterministic but order-dependent feature mappings

### Phase 3: Library Aggregation

**Functions**: `add-several-libraries()`, `export-library()`

**Library Source Options** (`option-libs-defs`):
- `"c"` - Current buffer only
- `"m"` - Exported libraries (from `/libs/` directory)
- `"s"` - Menu-selected libraries
- `"l"` - Include-book loaded libraries
- `"g"` - Global ACL2 library (all system files)

**Data Merging Process**:
```
Current Session Data:
  lemmas → lemmas-libraries
  definitions → definitions-libraries

Library Import:
  for each selected library:
    lemmas-libraries += import-library(lib)
    definitions-libraries += import-definitions(lib)

Feature Vector Generation:
  convert-recursive-several-libraries() → lemma-vectors
  convert-recursive-several-libraries-defs() → defs-vectors
```

### Phase 4: Machine Learning Integration

**Clustering Entry Points**:
- `cluster-general()` - Interactive clustering with user prompts
- `show-similarities-general()` - Find similarities for theorem at cursor

**Granularity to Cluster Count Mapping**:
```
granularity-level → cluster count calculation:
1: length/8 (largest clusters, weakest correlation)
2: length/7
3: length/5 (default)
4: length/4
5: length/2 (smallest clusters, strongest correlation)
```

**Weka Integration Pipeline**:
```
lemma-vectors/defs-vectors
  ↓ convert-to-weka-format()
CSV format
  ↓ add headers
ARFF format
  ↓ java -classpath weka.jar
Weka clustering results
  ↓ parse-results()
Display clusters/similarities
```

**Weka Command Template**:
```bash
java -classpath ${WEKA_JAR} weka.clusterers.${ALGORITHM}
     -N ${CLUSTER_COUNT}
     -t temp3.arff
     -p 0 > out.arff
```

## Critical Buffer Dependencies (MCP Blockers)

### 1. Expression Parsing Dependencies
- **`(buffer-substring beg end)`** - Extract ACL2 code from buffer
- **`(forward-sexp)`, `(backward-sexp)`** - Navigate S-expressions
- **`(point)`** - Determine processing scope
- **MCP Solution**: Direct string/file parsing with Lisp reader

### 2. ACL2 Session Management
- **`(comint-send-string *acl2-buffer-name* ...)`** - Send commands to ACL2
- **`(eval-this-event)`** - Evaluate expressions in ACL2
- **File communication via `tmp.out`** - Redirect ACL2 output
- **MCP Solution**: Direct ACL2 process management with pipes

### 3. Position-Based Analysis
- **`show-similarities-general()`** - Analyzes theorem at cursor position
- **`obtain-guards-theorem()`** - Extracts guards for theorem at cursor
- **MCP Solution**: Explicit theorem name/content parameters

### 4. Interactive Configuration
- **`(read-string ...)`** - Prompts for algorithm, granularity, library options
- **MCP Solution**: Function parameters in tool calls

## Complete Parameter Reference

### Clustering Algorithms
- **`"k"`** - SimpleKMeans (default)
- **`"e"`** - EM (Expectation-Maximization)
- **`"f"`** - FarthestFirst

### Granularity Levels (1-5)
- **`1`** - Low granularity (big clusters, weak correlation)
- **`2`** - Medium-low (length/7 clusters)
- **`3`** - Medium (default, length/5 clusters)
- **`4`** - Medium-high (length/4 clusters)
- **`5`** - High granularity (small clusters, strong correlation, length/2)

### Library Selection Modes
- **`"c"`** - Current library only
- **`"m"`** - Exported libraries (from `/libs/` directory)
- **`"s"`** - Menu-selected libraries
- **`"l"`** - Include-book loaded libraries
- **`"g"`** - Global ACL2 library (entire system)

### Content Types
- **`"d"`** - Cluster definitions (functions)
- **`"t"`** - Cluster theorems (lemmas)

### Boolean Flags
- **`whysimilar`** - Show similarity explanations in clustering output
- **`lemmaanalogy`** - Enable lemma dependency analysis

## MCP Service Architecture Design

### Core Architectural Principles

1. **Stateless Operations**: Each tool call should be independent
2. **Explicit Parameters**: No interactive prompts or cursor dependencies
3. **Batch Processing**: Process entire files/libraries in single operations
4. **Deterministic Features**: Consistent feature extraction regardless of order

### Proposed MCP Tools

#### `acl2_extract_features`
Extract feature vectors from ACL2 expressions.

**Parameters**:
```json
{
  "content": "string (ACL2 expressions)",
  "library_context": "array (imported library names)",
  "feature_dictionary": "object (existing arity mappings)",
  "content_type": "enum: ['theorems', 'definitions', 'mixed']"
}
```

**Returns**:
```json
{
  "features": [{"name": "theorem_name", "vector": [1,0,2,-1,0...]}],
  "updated_dictionary": {"arity0": [...], "arity1": [...], ...},
  "metadata": {"total_processed": 10, "new_features": 3}
}
```

#### `acl2_cluster_analysis`
Perform clustering analysis on ACL2 content.

**Parameters**:
```json
{
  "content": "string (ACL2 expressions) OR null (use libraries)",
  "content_type": "enum: ['definitions', 'theorems']",
  "algorithm": "enum: ['k', 'e', 'f']",
  "granularity": "integer: 1-5",
  "library_selection": "enum: ['current', 'exported', 'selected', 'loaded', 'global']",
  "library_paths": "array (for 'selected' mode)",
  "show_explanations": "boolean",
  "feature_dictionary": "object (arity mappings)"
}
```

**Returns**:
```json
{
  "clusters": [
    {
      "cluster_id": 0,
      "members": ["theorem1", "theorem2"],
      "centroid": [1.2, 0.8, ...],
      "explanation": "string (if show_explanations=true)"
    }
  ],
  "metadata": {"algorithm": "k", "total_items": 15, "cluster_count": 3}
}
```

#### `acl2_find_similarities`
Find theorems/definitions similar to a target.

**Parameters**:
```json
{
  "target_name": "string (theorem/definition name)",
  "target_content": "string (ACL2 expression)",
  "context_libraries": "array (library names/paths)",
  "algorithm": "enum: ['k', 'e', 'f']",
  "granularity": "integer: 1-5",
  "max_results": "integer: 1-20",
  "feature_dictionary": "object"
}
```

**Returns**:
```json
{
  "similarities": [
    {
      "name": "similar_theorem",
      "similarity_score": 0.85,
      "cluster_id": 2,
      "content": "ACL2 expression"
    }
  ],
  "target_cluster": 2
}
```

#### `acl2_generate_guards`
Generate guard conditions for incomplete theorems.

**Parameters**:
```json
{
  "theorem_content": "string (incomplete ACL2 theorem)",
  "acl2_context": "string (previous definitions)",
  "function_guards": "object (known function guard conditions)"
}
```

**Returns**:
```json
{
  "suggested_guards": [
    "(INTEGERP N)",
    "(NOT (< N 0))",
    "(ACL2-NUMBERP K)"
  ],
  "complete_theorem": "string (theorem with implies clause)"
}
```

#### `acl2_manage_libraries`
Export, import, and manage ACL2 libraries for reuse.

**Parameters**:
```json
{
  "action": "enum: ['export', 'import', 'list', 'delete']",
  "library_name": "string",
  "content": "string (for export)",
  "library_path": "string (for import)"
}
```

### MCP Resources

#### `acl2://libraries/{library_name}`
Cached processed libraries with feature vectors.
```json
{
  "name": "library_name",
  "theorems": [{"name": "...", "vector": [...]}],
  "definitions": [{"name": "...", "vector": [...]}],
  "feature_dictionary": {"arity0": [...], ...},
  "metadata": {"processed_date": "...", "acl2_version": "..."}
}
```

#### `acl2://clusters/{cluster_id}`
Cached clustering results for quick similarity queries.
```json
{
  "algorithm": "k",
  "granularity": 3,
  "clusters": [...],
  "feature_dictionary": {...},
  "source_libraries": ["lib1", "lib2"]
}
```

#### `acl2://features/{content_hash}`
Cached feature vectors to avoid recomputation.
```json
{
  "hash": "sha256_of_content",
  "features": [{"name": "...", "vector": [...]}],
  "extraction_metadata": {...}
}
```

## Implementation Roadmap

### Phase 1: Core Feature Extraction (Weeks 1-2)
1. Implement ACL2 expression parser (replace buffer navigation)
2. Port `extract-info`, `build-table`, `populate-table`, `flatten-table`
3. Create deterministic feature dictionary management
4. Implement `acl2_extract_features` tool

### Phase 2: ACL2 Integration (Weeks 3-4)
1. Replace comint with direct ACL2 process management
2. Implement guard extraction (`acl2_generate_guards`)
3. Add support for macro expansion and include-book processing

### Phase 3: Machine Learning Pipeline (Weeks 5-6)
1. Port Weka integration to direct Java process calls
2. Implement `acl2_cluster_analysis` and `acl2_find_similarities`
3. Add all parameter options and library selection modes

### Phase 4: Library Management (Week 7)
1. Implement `acl2_manage_libraries` tool
2. Add MCP resource-based caching
3. Support for global ACL2 library indexing

### Phase 5: Testing & Validation (Week 8)
1. Comprehensive testing against original Emacs extension
2. Performance optimization
3. Documentation and examples

## Testing Strategy

### Functional Equivalence Tests
- Compare feature vectors generated by MCP service vs original Emacs extension
- Verify clustering results match for same input data
- Test guard generation accuracy

### Performance Benchmarks
- Feature extraction speed for large libraries
- Clustering performance with different granularity levels
- Memory usage for global ACL2 library processing

### Integration Tests
- End-to-end workflow testing
- Library import/export functionality
- Error handling and edge cases

## Known Challenges & Solutions

### Challenge 1: Feature Dictionary Consistency
**Problem**: Original system has order-dependent feature mappings
**Solution**: Implement canonical ordering and deterministic initialization

### Challenge 2: ACL2 Session Management
**Problem**: Complex ACL2 interaction via Emacs comint
**Solution**: Direct process management with stdin/stdout pipes

### Challenge 3: Large Library Processing
**Problem**: Global ACL2 library contains thousands of files
**Solution**: Implement streaming processing and resource-based caching

### Challenge 4: Weka Java Integration
**Problem**: Shell command dependency on weka.jar
**Solution**: Bundle Weka or implement native clustering algorithms

This document serves as the definitive reference for understanding the original ACL2(ml) system and implementing its MCP adaptation. All implementation decisions should reference this analysis to ensure functional completeness and architectural consistency.
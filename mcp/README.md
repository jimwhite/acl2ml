# ACL2(ml) MCP Server

A modernized version of ACL2(ml) implemented as an MCP (Model Context Protocol) server for AI-assisted ACL2 theorem proving.

## Overview

ACL2(ml) provides machine learning-powered assistance for ACL2 theorem proving by:

- **Indexing ACL2 definitions** from the books library
- **Extracting features** from theorems and lemmas for ML analysis
- **Finding similar theorems** using cosine similarity and other metrics
- **Recommending useful lemmas** for proof goals
- **Analyzing theorem structure** and complexity

This version has been updated for ACL2 8.6 and reimplemented in Common Lisp as an MCP server.

## Installation

### Prerequisites

- SBCL or CCL Common Lisp implementation
- ACL2 8.6 with `saved_acl2` binary
- ACL2 books library
- Internet connection (for downloading dependencies)

### Setup

1. Install dependencies via Quicklisp:
```lisp
(ql:quickload '(:40ants-mcp :alexandria :cl-ppcre :parse-number))
```

2. Load the system:
```lisp
(asdf:load-system :acl2ml-mcp)
```

3. Configure paths (update as needed):
```lisp
(setf acl2ml-mcp:*acl2-binary-path* "/home/acl2/saved_acl2")
(setf acl2ml-mcp:*acl2-books-dir* "/home/acl2/books/")
```

## Usage

### Starting the Server

#### STDIO Transport (default)
```bash
sbcl --load start-server.lisp --stdio
```

#### HTTP Transport
```bash
sbcl --load start-server.lisp --http --port=8080
```

#### With Custom Paths
```bash
sbcl --load start-server.lisp --acl2-binary=/path/to/saved_acl2 --books-dir=/path/to/books/
```

### MCP Tools

The server provides these MCP tools:

#### `regenerate-definitions-index`
Create/update the definitions index from ACL2 books.

Parameters:
- `scan-all` (boolean): Scan all books in the books directory
- `books` (array): List of specific books to index

Example:
```json
{
  "method": "tools/call",
  "params": {
    "name": "regenerate-definitions-index",
    "arguments": {
      "books": ["arithmetic/top", "std/lists/top"]
    }
  }
}
```

#### `list-definitions`
List definitions from the index.

Parameters:
- `book-filter` (string): Filter by book name
- `type-filter` (string): Filter by type (defun, defthm, etc.)
- `limit` (integer): Maximum results (default: 20)

#### `analyze-theorem`
Analyze the structure of a theorem.

Parameters:
- `theorem-expression` (string): S-expression of the theorem

Example:
```json
{
  "method": "tools/call",
  "params": {
    "name": "analyze-theorem",
    "arguments": {
      "theorem-expression": "(defthm append-associative (implies (and (true-listp x) (true-listp y)) (equal (append (append x y) z) (append x (append y z)))))"
    }
  }
}
```

#### `extract-features`
Extract ML features from an ACL2 expression.

Parameters:
- `expression` (string): S-expression to analyze

#### `find-similar-lemmas`
Find lemmas similar to a given theorem.

Parameters:
- `theorem-expression` (string): Target theorem
- `similarity-threshold` (number): Minimum similarity (0.0-1.0, default: 0.7)
- `max-results` (integer): Maximum results (default: 5)

#### `recommend-lemmas`
Recommend lemmas useful for proving a theorem.

Parameters:
- `goal-theorem` (string): Theorem goal
- `max-recommendations` (integer): Maximum recommendations (default: 5)

#### `search-definitions`
Search definitions by pattern.

Parameters:
- `pattern` (string): Search pattern
- `field` (string): Field to search (name, book, type)
- `limit` (integer): Maximum results

#### `get-definition`
Get detailed information about a specific definition.

Parameters:
- `definition-name` (string): Name of the definition

#### `export-features-csv`
Export definition features to CSV for ML training.

Parameters:
- `filename` (string): Output filename (default: "acl2ml-features.csv")

## Architecture

### Components

- **`acl2-interface.lisp`**: Communication with ACL2 via `saved_acl2` binary
- **`definitions-index.lisp`**: Indexing and management of ACL2 definitions
- **`feature-extraction.lisp`**: ML feature extraction from S-expressions
- **`lemma-analysis.lisp`**: Theorem structure analysis and similarity computation
- **`mcp-tools.lisp`**: MCP tool definitions
- **`server.lisp`**: Main server implementation

### ACL2 Integration

The system communicates with ACL2 8.6 through the `saved_acl2` binary:

1. **Start ACL2 process** with `saved_acl2`
2. **Load books** using `include-book`
3. **Switch to Common Lisp** with `:q` for introspection
4. **Return to ACL2** with `(lp)` when needed

### Feature Extraction

The ML feature extraction process:

1. **Parse S-expressions** into structural components
2. **Extract symbol features**: arity, depth, frequency
3. **Compute structural metrics**: max depth, branching factor, node counts
4. **Calculate complexity scores**: symbol diversity, function calls
5. **Generate feature vectors** for similarity comparison

### Similarity Metrics

- **Cosine similarity** for feature vector comparison
- **Jaccard similarity** for dependency set comparison
- **Euclidean distance** for structural similarity
- **Combined scoring** using weighted averages

## Development

### Adding New Tools

1. Define the tool in `mcp-tools.lisp`:
```lisp
(40ants-mcp/tools:define-tool (acl2ml-tools my-new-tool)
    (param1 &key (param2 "default"))
  (:summary "Description of the tool")
  (:param param1 string "Description of param1")
  (:param param2 string "Description of param2")
  (:result (soft-list-of text-content))

  ;; Implementation
  (list (make-instance 'text-content :text "Result")))
```

2. Add any supporting functions to the appropriate module

### Testing

Start the test environment:
```lisp
(acl2ml-mcp:test-acl2ml-locally)
```

Run health checks:
```lisp
(acl2ml-mcp:health-check)
```

## Troubleshooting

### Common Issues

**"ACL2 binary not found"**
- Verify `*acl2-binary-path*` points to your `saved_acl2` binary
- Check that ACL2 8.6 is properly installed

**"Definitions index empty"**
- Run `regenerate-definitions-index` to build the index
- Check that `*acl2-books-dir*` points to your ACL2 books

**"Timeout waiting for ACL2 prompt"**
- Increase `*acl2-timeout*` for slower systems
- Check ACL2 binary is working: `./saved_acl2`

### Debugging

Enable detailed logging:
```lisp
(setf *debug-io* *standard-output*)
```

Check server configuration:
```lisp
(acl2ml-mcp:show-acl2ml-config)
```

## License

MIT License - Updated for ACL2 8.6 and MCP integration.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

For questions or issues, please use the GitHub issue tracker.
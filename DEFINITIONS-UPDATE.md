# ACL2(ml) Definitions Update Guide

The `definitions/` directory contains indexed ACL2 book definitions from ACL2 2.6. 
For ACL2 8.6, you have several options:

## Quick Solution (Recommended)

1. **Use current library only**: When clustering, choose option `c` (current library) instead of `g` (global library). This will only analyze your current ACL2 file, bypassing the outdated definitions.

2. **Clear outdated definitions**: 
   ```bash
   cd /workspaces/acl2ml
   mv definitions definitions-old-acl2-2.6
   mkdir definitions
   mkdir definitions/global
   ```

## Generate New Definitions for Books You Use

When you use `include-book` statements in your ACL2 files, you can generate definitions for those specific books:

1. **Open a book file** (e.g., `/home/acl2/books/arithmetic-5/top.lisp`)
2. **Load ACL2(ml)**: Make sure ACL2(ml) is loaded
3. **Export the library**: Use `M-x acl2ml-export-library` or `Ctrl-C Ctrl-E`
4. This will create definition files in `/workspaces/acl2ml/definitions/` and `/workspaces/acl2ml/libs/`

## Regenerate Common Books (Advanced)

If you want to rebuild the global index for commonly used books:

1. Load the regeneration script:
   ```elisp
   (load-file "/workspaces/acl2ml/regenerate-definitions.el")
   ```

2. Run the regeneration:
   ```elisp
   M-x regenerate-acl2ml-definitions
   ```

## What Each Clustering Option Means

When you press `Ctrl-C Ctrl-C` and choose `d` or `t`, then choose:
- `c` = Current library (your current file) - **RECOMMENDED**
- `m` = Libraries you have exported manually  
- `s` = Libraries selected in menu
- `l` = Libraries that you have loaded (via include-book)
- `g` = Global library (the outdated ACL2 2.6 books) - **AVOID**

## Recommendation

For now, use option `c` (current library) when clustering. This will analyze your current ACL2 file without relying on the outdated global definitions.

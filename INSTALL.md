# ACL2(ml) Installation Instructions

## Prerequisites

- ACL2 8.6 (or compatible version)
- GNU Emacs
- Java (for Weka clustering functionality)

## Installation

1. **Clone or download ACL2(ml) to your system**

2. **Add to your Emacs configuration**
   
   Add the following line to your `~/.emacs` file (or `~/.emacs.d/init.el`):
   
   ```elisp
   (load-file "/workspaces/acl2ml/main.el")
   ```
   
   **Important**: Update the path to match where you installed ACL2(ml) on your system.

3. **Configure paths (if needed)**
   
   If your ACL2 installation is not at `/home/acl2/saved_acl2`, edit `main.el` and update the `*acl2-dir*` constant:
   
   ```elisp
   (defconst *acl2-dir* "/path/to/your/acl2/executable")
   ```

## Usage

1. **Automatic loading**: ACL2(ml) will automatically activate when you open `.lisp` files in Emacs.

2. **Manual start**: You can also start ACL2(ml) manually with:
   ```
   M-x start-acl2ml
   ```

3. **Key bindings**:
   - `Ctrl-C Ctrl-T`: Evaluate next ACL2 event
   - `Ctrl-C Ctrl-C`: Run clustering analysis
   - `Ctrl-C Ctrl-U`: Evaluate up to current point
   - `Ctrl-C Ctrl-G`: Obtain guards for theorem

## Troubleshooting

- Ensure ACL2 is properly installed and accessible
- Verify that the paths in `main.el` are correct for your system
- Check that Java is installed for Weka clustering functionality

## Compatibility

This version has been updated to work with ACL2 8.6, fixing compatibility issues from the original ACL2 2.6 version.

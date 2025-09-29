# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Configuration Structure

This is a Doom Emacs literate configuration that uses Org mode for documentation and configuration management. The main configuration is written in `config.org`, which is tangled to generate `config.el`.

## Key Commands

```bash
# Sync Doom configuration after modifying init.el or packages.el
doom sync

# Update Doom packages
doom upgrade

# Tangle config.org to regenerate config.el (automatically done by Doom)
# Manual tangle: In Emacs: C-c C-v t (or SPC m b t in evil mode)

# Access Doom documentation
# In Emacs: SPC h d h (or C-h d h for non-vim users)

# View module documentation
# In Emacs: K on module name (or C-c c k for non-vim users)
```

## Configuration Architecture

- **config.org**: Literate configuration source file containing all Emacs Lisp code blocks that get tangled to config.el. Also symlinked as README.org for GitHub display
- **init.el**: Declares which Doom modules are enabled (completion, ui, editor, tools, languages, etc.)
- **packages.el**: Declares additional packages to install beyond Doom's defaults (currently includes org-super-agenda)
- **config.el**: Generated from config.org - do not edit directly

## Module System

The configuration uses Doom's module system with these enabled categories:
- **Completion**: corfu with orderless, vertico
- **UI**: doom theme, dashboard, hl-todo, ligatures, treemacs, smooth scrolling, vc-gutter with pretty flag
- **Editor**: evil mode (vim keybindings), file templates, code folding, snippets
- **Tools**: magit for git, eval with overlay, lookup for code navigation, tree-sitter
- **Languages**: emacs-lisp, javascript (+tree-sitter), python, markdown, org (+pretty), shell scripting, web, yaml, data formats (json)
- **Email**: mu4e with org and gmail support
- **App**: RSS reader with org integration

## Custom Functions and Keybindings

Custom functions use the `adaen/` prefix namespace. Key leader prefix is `SPC` in evil mode.

Notable custom keybindings:
- `SPC -`: Comment line
- `SPC t e`: Toggle eshell split
- `SPC t l`: Toggle line numbers
- `SPC t m`: Toggle markdown view mode
- `SPC t t`: Toggle truncate lines
- `SPC t T`: Toggle treemacs
- `SPC t v`: Toggle vterm split
- `SPC o e`: Open eshell here (full frame)
- `SPC o v`: Open vterm here (full frame)

## Org Mode Configuration

### TODO Keywords
- Regular tasks: TODO → NEXT → RECURRING → WAITING → DONE
- Projects: PROJECT → PROJECT-HOLD → PROJECT-DONE

### Project State Automation
The `adaen/update-project-state` function automatically manages project states:
- PROJECT → PROJECT-HOLD when has WAITING child and no NEXT children
- Any state → PROJECT-DONE when all children are DONE
- PROJECT-HOLD → PROJECT when NEXT child added

### Org Files
- Main agenda file: `~/org/gtd/main.org`
- Archive location: `~/org/gtd/archive.org`

## Custom Functions

- **adaen/toggle-markdown-view-mode**: Switches between markdown-mode (editing) and markdown-view-mode (reading)
- **adaen/update-project-state**: Auto-updates PROJECT states based on child task completion

## Theming

Uses `doom-palenight` theme with custom font settings (Iosevka SS04 at size 15) and customized header sizes for both Org and Markdown modes. Headers scale progressively from 1.4x (h1) down to 0.7x (h8) for Org mode.

## Commit Message Best Practices

Keep commit messages simple and clear. Focus on clarity over fluff - a good commit message explains what changed and why in the fewest words possible. Avoid unnecessary adjectives or lengthy explanations.

## Best Practices for config.org Modifications

### Literate Configuration Guidelines

1. **Section Organization**: Add new configurations to the most appropriate existing section. If no section fits, create a new one with a clear, descriptive name.

2. **Documentation First**: Every code block should be preceded by a description explaining:
   - What the configuration does
   - Why it's needed
   - Any dependencies or requirements
   - Example usage (for functions and keybindings)

3. **Code Block Structure**:
   ```org
   ** Section Title
   Brief description of what this section configures and why.
   #+begin_src emacs-lisp
   ;; Your elisp code here
   #+end_src
   ```

### Code Organization Rules

1. **Section Placement**:
   - Core Settings: Fundamental Emacs/Doom settings (theme, font, basic behavior)
   - Appearance: Visual customizations (faces, themes, UI elements)
   - Keybindings: Custom key mappings (use `map!` macro)
   - [Mode] sections: Mode-specific configurations (Org Mode, Markdown, etc.)
   - Terminal: Terminal/shell related settings
   - System Settings: OS-level or Emacs system settings

2. **Naming Conventions**:
   - Custom functions: Always use `adaen/` prefix (e.g., `adaen/toggle-feature`)
   - Variables: Use descriptive names, avoid single letters
   - Keybinding descriptions: Start with verb ("Toggle", "Open", "Insert")

3. **Load Order Considerations**:
   - Use `after!` macro when configuring packages to ensure they're loaded first
   - Place fundamental settings before dependent configurations
   - Group related settings together

### Common Patterns and Examples

#### Adding a New Toggle Function
```emacs-lisp
(defun adaen/toggle-feature-name ()
  "Toggle between two states of feature."
  (interactive)
  (if condition
      (enable-action)
    (disable-action)))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle feature" "x" #'adaen/toggle-feature-name))
```

#### Configuring a Package
```emacs-lisp
(after! package-name
  (setq package-variable value)
  (add-hook! 'package-mode-hook
    (defun configure-package-h ()
      ;; Configuration code
      )))
```

#### Adding Mode-Specific Settings
```emacs-lisp
(after! mode-name
  ;; Mode variables
  (setq mode-specific-var value)
  ;; Mode hooks
  (add-hook 'mode-name-hook #'function-to-run))
```

### After Making Changes

When modifying config.org:
1. The file will be automatically tangled to config.el when Doom restarts
2. Run `doom sync` if packages.el or init.el were modified
3. User will handle testing and reloading

### Package Management

When your configuration requires new packages:

1. **Check if package exists in Doom**:
   - Review init.el modules first
   - Check if Doom provides the functionality

2. **Adding to packages.el**:
   ```emacs-lisp
   ;; In packages.el
   (package! package-name)  ; From MELPA
   (package! package-name :recipe (...))  ; Custom recipe
   ```

3. **After adding packages**:
   ```bash
   doom sync  # Install new packages
   doom doctor  # Check for issues
   ```

### Elisp Style Guide

1. **Indentation**: Let Emacs handle it (use `indent-region`)
2. **Comments**:
   - Single `;` for end-of-line
   - Double `;;` for line comments
   - Triple `;;;` for section headers
3. **Line Length**: Keep under 80 characters when possible
4. **Parentheses**: Close on same line for short forms, new line for long blocks
5. **Quotes**: Use `'` for symbols, `"` for strings

### Keybinding Conventions

1. **Leader Key Prefixes** (maintain consistency with these):
   - `SPC t`: Toggles
   - `SPC o`: Open/launch
   - `SPC s`: Search
   - `SPC f`: Files
   - `SPC b`: Buffers
   - `SPC w`: Windows
   - `SPC p`: Projects

2. **When Adding New Keybindings**:
   - Use mnemonic keys (m for markdown, t for terminal)
   - Avoid overriding Doom defaults
   - Follow existing patterns in the config

### Important Considerations

1. **Syntax**: Ensure balanced parentheses and proper quoting
2. **Load Order**: Use `after!` for package-specific settings
3. **Package Dependencies**: Add to packages.el before using in config.org
4. **Namespace**: Always prefix custom functions with `adaen/`
5. **Documentation**: Every code block needs explanatory text

### Table of Contents Maintenance

The TOC in config.org uses the `:toc:` tag. When adding new sections:
- Add them with appropriate heading levels (`*`, `**`, etc.)
- The TOC will be regenerated automatically when the file is saved in Emacs
- Maintain consistent heading hierarchy for proper TOC structure
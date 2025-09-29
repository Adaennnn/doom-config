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
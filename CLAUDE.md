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

# Access Doom documentation
# In Emacs: SPC h d h (or C-h d h for non-vim users)

# View module documentation
# In Emacs: K on module name (or C-c c k for non-vim users)
```

## Configuration Architecture

- **config.org**: Literate configuration source file containing all Emacs Lisp code blocks that get tangled to config.el
- **init.el**: Declares which Doom modules are enabled (completion, ui, editor, tools, languages, etc.)
- **packages.el**: Declares additional packages to install beyond Doom's defaults
- **config.el**: Generated from config.org - do not edit directly

## Module System

The configuration uses Doom's module system with these enabled categories:
- **Completion**: corfu with orderless, vertico
- **UI**: doom theme, dashboard, ligatures, treemacs, smooth scrolling
- **Editor**: evil mode (vim keybindings), file templates, code folding, snippets
- **Tools**: magit for git, eval with overlay, lookup for code navigation
- **Languages**: emacs-lisp, javascript, python, markdown, org-mode, shell scripting, web, yaml

## Custom Functions and Keybindings

Custom functions use the `adaen/` prefix namespace. Key leader prefix is `SPC` in evil mode.

Notable custom keybindings:
- `SPC -`: Comment line
- `SPC t e`: Toggle eshell
- `SPC t m`: Toggle markdown view mode
- `SPC t v`: Toggle vterm

## Theming

Uses `doom-palenight` theme with custom font settings (Iosevka SS04) and customized header sizes for both Org and Markdown modes.
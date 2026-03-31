# AGENTS.md

## Configuration Structure

This is a Doom Emacs literate configuration. The `:config literate` module in init.el enables config.org to be automatically tangled to config.el on Doom restart. **Never edit config.el directly** - all changes go in config.org.

- **config.org**: Literate configuration source (also symlinked as README.org for GitHub). All Emacs Lisp code blocks are tangled to config.el
- **init.el**: Declares enabled Doom modules
- **packages.el**: Additional packages beyond Doom's defaults
- **config.el**: Auto-generated from config.org - do not edit

## Key Commands

```bash
# Sync after modifying init.el or packages.el
doom sync
```

When adding new packages: add to packages.el first, then run `doom sync` before using in config.org.

## Editing config.org

### Section Layout

The file is organized in this order - add new configurations to the appropriate section:

1. **Core Settings** - theme, font, fundamental Doom settings
2. **Appearance** - face customizations (org headers, markdown headers, agenda headers)
3. **Keybindings** - custom key mappings using `map!` macro
4. **Org Mode** - org-specific config (basic settings, super-agenda, project automation, archiving)
5. **Markdown** - markdown-specific config
6. **Terminal** - vterm/eshell settings
7. **System Settings** - performance, auto-save, UI/display (always last)

### Code Block Format

Each section should have descriptive text before its code block:

```org
** Section Title
Brief description of what this configures and why.
#+begin_src emacs-lisp
;; elisp code here
#+end_src
```

### Conventions

- Custom functions: prefix with `adaen/` (e.g., `adaen/toggle-feature`)
- Keybinding descriptions: start with a verb ("Toggle", "Open", "Archive")
- Use `after!` macro when configuring packages to ensure correct load order
- Use `map!` macro for keybindings, following existing prefix patterns
- Elisp comments: `;` end-of-line, `;;` line comments, `;;;` section headers
- Keep lines under 80 characters when possible
- The TOC uses the `:toc:` tag and auto-regenerates on save in Emacs

### Leader Key Prefixes

Maintain consistency with these existing prefixes:

| Prefix  | Purpose           | Examples                                               |
|---------|-------------------|--------------------------------------------------------|
| `SPC t` | Toggles           | eshell, line numbers, markdown view, truncate, treemacs, vterm |
| `SPC o` | Open (full frame) | eshell here, vterm here (Doom defaults also live here) |
| `SPC m` | Local leader      | `SPC m a` archives completed tasks in org-mode         |
| `SPC -` | Comment line      |                                                        |

### Adding a Package

1. Check if Doom already provides the functionality (review init.el modules)
2. Add `(package! package-name)` to packages.el
3. Run `doom sync`
4. Configure in config.org using `(after! package-name ...)` or `(use-package! ...)`

### After Making Changes

1. config.org tangles automatically when Doom restarts
2. Run `doom sync` only if packages.el or init.el were modified
3. User handles testing and reloading

## Org Mode GTD System

### TODO Keywords

- Tasks: `TODO` → `NEXT` → `RECURRING` → `WAITING` → `DONE`
- Projects: `PROJECT` → `PROJECT-HOLD` → `PROJECT-DONE`

### Project State Automation

`adaen/update-project-state` runs on `org-after-todo-state-change-hook`:
- PROJECT → PROJECT-HOLD: has WAITING child, no NEXT children
- Any → PROJECT-DONE: all children are DONE
- PROJECT-HOLD → PROJECT: NEXT child added

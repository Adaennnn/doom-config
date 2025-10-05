;;; config.el -*- lexical-binding: t -*-
(setq doom-theme 'doom-vibrant)

(setq doom-font (font-spec :family "Iosevka SS04" :size 28 :weight 'regular))

(custom-theme-set-faces!
'doom-vibrant
'(org-level-8 :inherit outline-3 :height 0.7)
'(org-level-7 :inherit outline-3 :height 0.8)
'(org-level-6 :inherit outline-3 :height 0.9)
'(org-level-5 :inherit outline-3 :height 1.0)
'(org-level-4 :inherit outline-3 :height 1.1)
'(org-level-3 :inherit outline-3 :height 1.2)
'(org-level-2 :inherit outline-2 :height 1.3)
'(org-level-1 :inherit outline-1 :height 1.4)
'(org-document-title :height 1.6 :bold t :underline nil))

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "Iosevka SS04"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 0.9)))))

(custom-set-faces
 '(org-super-agenda-header ((t (:foreground "#51afef" :weight bold :height 1.1)))))

(map! :leader
      :desc "Comment line" "-" #'comment-line)

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle eshell split"            "e" #'+eshell/toggle
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle markdown-view-mode"      "m" #'adaen/toggle-markdown-view-mode
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines
       :desc "Toggle treemacs"                "T" #'+treemacs/toggle
       :desc "Toggle vterm split"             "v" #'+vterm/toggle))

(map! :leader
      (:prefix ("o" . "open here")
       :desc "Open eshell here"    "e" #'+eshell/here
       :desc "Open vterm here"     "v" #'+vterm/here
       :desc "Archive completed tasks" "z" #'adaen/archive-completed-tasks))

(after! org
  (setq org-directory "~/org/")

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "NEXT(n)"
           "RECURRING(r)"
           "WAITING(w)"
           "|"
           "DONE(d!)")
          (sequence
           "PROJECT(p)"
           "PROJECT-HOLD(h)"
           "|"
           "PROJECT-DONE(D)")))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "#7f8c8d" :weight normal)
          ("NEXT" :foreground "#27ae60" :weight normal)
          ("RECURRING" :foreground "#3498db" :weight normal)
          ("WAITING" :foreground "#e67e22" :weight normal)
          ("PROJECT" :foreground "#9b59b6" :weight normal)
          ("PROJECT-HOLD" :foreground "#f39c12" :weight normal)))

  (setq org-agenda-files '("~/org/gtd/main.org"))

  (setq org-archive-location "~/org/gtd/archive.org::datetree/"
        org-archive-subtree-save-file-p t)

  (setq org-tag-alist
        '(;; Contexts
          ("@home" . ?h)
          ("@office" . ?o)
          ("@computer" . ?c)
          ("@phone" . ?p)
          ("@errands" . ?e)
          ("@anywhere" . ?a)

          ;; Energy levels
          ("@high_energy" . ?1)
          ("@medium_energy" . ?2)
          ("@low_energy" . ?3)

          ;; Time estimates
          ("@quick" . ?4)      ; < 15m
          ("@short" . ?5)      ; 15-60m
          ("@medium" . ?6)     ; 1-4h
          ("@long" . ?7))))

(defun adaen/org-agenda-clean-prefix ()
  "Return empty string for clean agenda display without filenames or project names."
  "")

(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)

  ;; Set custom agenda prefix format for clean display
  (setq org-agenda-prefix-format
        '((agenda . " %i %(adaen/org-agenda-clean-prefix)%?-12t% s")
          (todo . " %i %(adaen/org-agenda-clean-prefix)")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))

  ;; Customize deadline/scheduled text
  (setq org-agenda-scheduled-leaders '("" ""))
  (setq org-agenda-deadline-leaders '("Deadline: " "In %d days: " "Overdue %d days: "))

  (setq org-agenda-custom-commands
        '(("d" "Day View"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-start-day ".")
                     (org-deadline-warning-days 7)
                     (org-scheduled-past-days 7)
                     (org-super-agenda-groups
                      '((:name "Overdue"
                         :deadline past
                         :scheduled past
                         :order 1)
                        (:name "Today"
                         :time-grid t
                         :date today
                         :deadline today
                         :scheduled today
                         :order 2)
                        (:name "Upcoming"
                         :deadline future
                         :scheduled future
                         :order 3)))))))

          ("w" "Week View"
           ((agenda ""
                    ((org-agenda-span 7)
                     (org-agenda-start-day ".")
                     (org-agenda-start-on-weekday 0)))))

          ("n" "Next Actions"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")))))

          ("W" "Waiting For"
           ((todo "WAITING"
                  ((org-agenda-overriding-header "Waiting For")))))

          ("p" "Projects"
           ((todo "PROJECT|PROJECT-HOLD"
                  ((org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "Active Projects"
                         :todo "PROJECT")
                      (:name "On Hold"
                         :todo "PROJECT-HOLD")))))))

          ("c" "Contexts"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "Computer"
                         :tag "@computer")
                      (:name "Phone"
                         :tag "@phone")
                      (:name "Home"
                         :tag "@home")
                      (:name "Office"
                         :tag "@office")
                      (:name "Errands"
                         :tag "@errands")
                      (:name "Anywhere"
                         :tag "@anywhere")))))))

          ("e" "Energy-based"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "High Energy"
                         :tag "@high_energy")
                      (:name "Medium Energy"
                         :tag "@medium_energy")
                      (:name "Low Energy"
                         :tag "@low_energy")))))))

          ("t" "Time Available"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "Quick (< 15 min)"
                         :tag "@quick")
                      (:name "Short (15-60 min)"
                         :tag "@short")
                      (:name "Medium (1-4 hours)"
                         :tag "@medium")
                      (:name "Long (4+ hours)"
                         :tag "@long")))))))))

  ;; Additional super-agenda settings
  (setq org-super-agenda-header-map nil)) ; Disable super-agenda keybindings

(after! org
  (setq org-capture-bookmark nil) ; Disable bookmark creation on capture
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry
                 (file "~/org/gtd/inbox.org")
                 "* %?\n"
                 :prepend nil)))

(defun adaen/update-project-state ()
  "Auto-update PROJECT states based on child tasks.
  - PROJECT → PROJECT-HOLD: when has WAITING child and no NEXT children
  - Any → PROJECT-DONE: when all children are DONE
  - PROJECT-HOLD → PROJECT: when NEXT child added"
  (save-excursion
    (org-back-to-heading t)
    (when (member (org-get-todo-state) '("PROJECT" "PROJECT-HOLD"))
      (let ((has-next nil)
            (has-waiting nil)
            (has-active nil)
            (all-done t))
        (org-map-entries
         (lambda ()
           (let ((state (org-get-todo-state)))
             (when state
               (cond
                ((string= state "NEXT") (setq has-next t all-done nil))
                ((string= state "WAITING") (setq has-waiting t all-done nil))
                ((member state '("TODO" "RECURRING"))
                 (setq has-active t all-done nil))))))
         nil 'tree)
        (cond
         (all-done
          (org-todo "PROJECT-DONE"))
         ((and has-waiting (not has-next))
          (org-todo "PROJECT-HOLD"))
         ((and (string= (org-get-todo-state) "PROJECT-HOLD") has-next)
          (org-todo "PROJECT")))))))

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (org-up-heading-safe)
              (adaen/update-project-state))))

(defun adaen/archive-completed-tasks ()
  "Archive completed tasks based on their context.
  - PROJECT-DONE projects: Archive entire subtree with all children
  - DONE tasks under 'Standalone Tasks': Archive individual task
  - DONE tasks under 'Recurring Tasks': Archive individual task
  - Skips DONE children of incomplete projects"
  (interactive)
  (let ((archived-count 0))
    (org-map-entries
     (lambda ()
       (let* ((state (org-get-todo-state))
              (level (org-current-level))
              (parent-heading (save-excursion
                               (when (org-up-heading-safe)
                                 (org-get-heading t t t t)))))
         (cond
          ;; Archive PROJECT-DONE at level 2 (projects)
          ((and (string= state "PROJECT-DONE")
                (= level 2))
           (org-archive-subtree)
           (setq archived-count (1+ archived-count))
           (setq org-map-continue-from (point)))

          ;; Archive DONE tasks under "Standalone Tasks" or "Recurring Tasks"
          ((and (string= state "DONE")
                (member parent-heading '("Standalone Tasks" "Recurring Tasks")))
           (org-archive-subtree)
           (setq archived-count (1+ archived-count))
           (setq org-map-continue-from (point))))))
     nil 'file)
    (message "Archived %d item(s)" archived-count)))

(defun adaen/toggle-markdown-view-mode ()
  "Toggle between `markdown-mode' and `markdown-view-mode'."
  (interactive)
  (if (eq major-mode 'markdown-view-mode)
      (markdown-mode)
    (markdown-view-mode)))

;; Disable line numbers
(after! vterm
  (add-hook! 'vterm-mode-hook
    (defun disable-line-numbers-h ()
      (display-line-numbers-mode -1))))

(setq auto-save-default t
      auto-save-timeout 20  ;; auto-save after 20 seconds idle
      auto-save-interval 200) ;; auto-save after 200 keystrokes

(setq confirm-kill-emacs nil) ;; Don't confirm on exit
(setq display-line-numbers-type t) ;; Turn line numbers on

;; Forces Emacs to start in fullscreen
(when (display-graphic-p)
  (setq initial-frame-alist
        '((fullscreen . maximized)))
  (setq default-frame-alist
        '((fullscreen . maximized))))

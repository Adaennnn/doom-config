(setq doom-theme 'doom-palenight)

(setq doom-font (font-spec :family "Iosevka SS04" :size 15 :weight 'regular))

(custom-theme-set-faces!
'doom-palenight
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
       :desc "Open vterm here"     "v" #'+vterm/here))

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

  (setq org-archive-location "~/org/gtd/archive.org::* %s")

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

(setq confirm-kill-emacs nil) ;; Don't confirm on exit
(setq display-line-numbers-type t) ;; Turn line numbers on
;; Forces Emacs to start in fullscreen
(when (display-graphic-p)
  (setq initial-frame-alist
        '((fullscreen . maximized)))
  (setq default-frame-alist
        '((fullscreen . maximized))))

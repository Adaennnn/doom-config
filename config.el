(setq doom-theme 'doom-palenight)

(setq doom-font (font-spec :family "Iosevka SS04" :size 15 :weight 'regular))

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

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "Iosevka SS04"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 0.9)))))

(defun adaen/toggle-markdown-view-mode ()
  "Toggle between `markdown-mode' and `markdown-view-mode'."
  (interactive)
  (if (eq major-mode 'markdown-view-mode)
      (markdown-mode)
    (markdown-view-mode)))

(after! org
  (setq org-directory "~/org/")
  
  (setq org-todo-keywords
        '((sequence 
           "TODO(t)"
           "NEXT(n!)"
           "RECURRING(r)"
           "WAITING(w@/!)"
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
  
  (setq org-archive-location "~/org/gtd/archive.org::* %s"))

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

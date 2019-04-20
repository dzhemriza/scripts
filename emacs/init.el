;;
;; File Name: init.el
;; Description: Emacs distro initialization
;;

;; Set custom lisp directory
(add-to-list 'load-path (concat (file-name-directory load-file-name)
				"lisp"))

;; customize theme (Note: search for theme above)
;;(load-theme 'wombat t)

;; Customization for removing toolbars, scrollbars and menu
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-cursor-color "#ffffff")
(blink-cursor-mode 0)

;;; Font size
(set-face-attribute 'default nil :height 90)

;; Enable sub-word mode for movement in emacs
(global-subword-mode 1)

;; Customization to enable line numbers of file
(global-linum-mode 1)

;; Show the column number
(setq column-number-mode t)

;; parent mode - highlight matching brackets
(show-paren-mode 1)

;; Allow navigation between the opened windows using Shift+[Left, Right, Up, Down]
(windmove-default-keybindings)

;; Deactivate linum-mode (line numbering) for shells only
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
				      term-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))

(ad-activate 'linum-on)

;; Enable IDO mode and all related interactive stuff
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Disable spash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Loads interactive imenu script
(require 'idomenu)

;; IDO GO TO SYMBOL used to jump very fast between function definitions
;; Copied from EmacsWiki
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol
		     (ido-completing-read "Symbol? " symbol-names))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	  (add-to-list 'name-and-pos (cons name position))))))))

;; Set global key
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;; ibuffer + global
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Fix for java mode for annotations
;;(require 'java-mode-indent-annotations)

;; Initialize smart eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; Delete all trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Very usefull customization that is used in cases when your are
;; inserting {} and we need to add new line above and ident
;; automatically
(defun open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

;; Set C-RET to automaticaly open new line above and ident
(global-set-key (kbd "<C-return>") 'open-line-above)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sr-speedbar (currently disabled)

;;(require 'sr-speedbar)
;;(setq speedbar-use-images nil)
;;(global-set-key [f8] 'sr-speedbar-toggle)
;;(sr-speedbar-refresh-turn-off)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(ede-project-directories (quote (file-name-directory load-file-name)))
 )
;; '(speedbar-show-unknown-files t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Untabify

(defun c++-mode-untabify ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
	(untabify (1- (point)) (point-max))))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++

(setq-default indent-tabs-mode nil)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 2) ;; Hard code identiation offset
  (c-set-offset 'substatement-open 0))
;;		indent-tabs-mode nil))
(add-hook 'c++-mode-hook
	  'my-c++-mode-hook
	  '(lambda ()
	     (make-local-hook 'write-contents-hooks)
	     (add-hook 'write-contents-hooks 'c++-mode-untabify)))

;; CEDET
(semantic-mode 0)

;;;;; emacs slow on sshfs
;; here is the fix

;; Disable git backend to speed up sshfs file load among other things
(setq vc-handled-backends ())

(require 'my-utils)
(require 'tramp)

;; Other custom stuff here

(message "...done!")

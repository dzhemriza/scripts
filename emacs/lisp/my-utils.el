;;
;; File Name: my-utils.el
;; Description: General Utilities
;;

(defun driza-open-shell (name dir)
  "Open new shell window in specific directory with specific file name"
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell name)))

(defun driza-open-eshell (name dir)
  "Open new eshell window in specific directory with specific file name"
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (eshell name)))

(defun split-window-multiple-ways (x y)
  "Split the current frame into a grid of X columns and Y rows."
  (interactive "nColumns: \nnRows: ")
  ;; one window
  (delete-other-windows)
  (dotimes (i (1- x))
    (split-window-horizontally)
    (dotimes (j (1- y))
      (split-window-vertically))
    (other-window y))
  (dotimes (j (1- y))
    (split-window-vertically))
  (balance-windows))

(autoload 'windmove-find-other-window "windmove"
"Return the window object in direction DIR.

\(fn dir &optional arg window)")

(declare-function windmove-find-other-window "windmove" (dir &optional arg window))

(defun get-window-in-frame (x y &optional frame)
  "Find Xth horizontal and Yth vertical window from top-left of FRAME."
  (let ((orig-x x) (orig-y y)
        (w (frame-first-window frame)))
    (while (and (windowp w) (> x 0))
      (setq w (windmove-find-other-window 'right 1 w)
            x (1- x)))
    (while (and (windowp w) (> y 0))
      (setq w (windmove-find-other-window 'down 1 w)
            y (1- y)))
    (unless (windowp w)
      (error "No window at (%d, %d)" orig-x orig-y))
    w))

(defun set-window-buffer-in-frame (x y buffer &optional frame)
  "Set Xth horizontal and Yth vertical window to BUFFER from top-left of FRAME."
  (set-window-buffer (get-window-in-frame x y frame) buffer))

;; =============================================================================
;; HMS project specific utility functions (deprecated)
;; =============================================================================

(defun driza-open-my-shells ()
  "Open all required shells for HMS development"
  (interactive)
  (let ((src-root (getenv "HMS_SRC_ROOT")))
    (message "HMS main src root: %s" src-root)

    ;; Build 2 sequences one for shell names and one for shell paths
    (setq shell-names '("*hcs-shell*"
			"*server-shell*"
			"*vcd-gateway-shell*"
			"*rest-model-shell*"
			"*main-shell*"
			"*hms1-run-shell*"
			"*hms2-run-shell*"
			"*hcs-run-shell*"
			))

    (setq shell-dirs '("/hms/hcs/"
		       "/hms/server/"
		       "/hms/jvsl/vcd-gateway/"
		       "/hms/interfaces/hcs-rest-model/"
		       "/hms/"
		       "/hms/server/"
		       "/hms/server/"
		       "/hms/hcs/"
		       ))

    (let ((i 0))
      (while (< i (length shell-names))
	(let ((name (elt shell-names i))
	      (dir (concat src-root (elt shell-dirs i))))
	  (message
	   "start open shell with name: %s path: %s"
	   name
	   dir)

	  (driza-open-shell name dir)
	)
	(setq i (+ i 1))))
    )

  ;; Open non project path related shells
  (driza-open-shell "*scripts*" "~/scripts/")
  )

(defun driza-hms-run-view ()
  "Load all used shells using driza-open-my-shells function.
And then split view horizontally in order to place *-run-shells"
  (interactive)
  (driza-open-my-shells)
  (split-window-multiple-ways 1 3)
  (set-window-buffer-in-frame 0 0 (get-buffer "*hms1-run-shell*"))
  (set-window-buffer-in-frame 0 1 (get-buffer "*hms2-run-shell*"))
  (set-window-buffer-in-frame 0 2 (get-buffer "*hcs-run-shell*"))
)

(defun driza-ib-split-view ()
  "Split emacs in ib view"
  (interactive)
  (split-window-multiple-ways 4 2)
  (balance-windows)
)

(defun driza-ib-split-min-view ()
  "Split emacs in ib view"
  (interactive)
  (split-window-multiple-ways 3 2)
  (balance-windows)
)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )


(defun driza-fast-down ()
  "Fast move down"
  (interactive)
  (forward-line 12)
)

(defun driza-fast-up ()
  "Fast move down"
  (interactive)
  (forward-line -12)
  )

(global-set-key (kbd "M-]") 'driza-fast-down)
(global-set-key (kbd "M-[") 'driza-fast-up)

(message "my-utils: done!")
(provide 'my-utils)

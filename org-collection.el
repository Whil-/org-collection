;;; org-collection.el -*- lexical-binding: t; -*-

;;; Code:
;;; Customizations
(require 'org)

(defvar org-collection-active nil
  "Variable used for determining the active org-collection.

org-collection-active should be either nil or one of the elements
in `org-collection-directories'.")

(defvar org-collection-file ".org-collection"
  "Customization file name for a collection.

To reside in the directory root of a collection.")

(defgroup org-collection nil
  "Options concerning configuring Org collections."
  :tag "Org Collection"
  :group 'org)

(defcustom org-collection-directories nil
  "Directories in which Org collection may search for files.
If the value is t, Org collection may search in any directory.

If the value is a function, Org collection calls that function
with one argument, the directory name; the function should return
t if Org collection should look for files in the directory.

Otherwise, the value should be a list of fully-expanded directory
names. Org collection searches for files only in those
directories.  If you invoke the commands \\[ede] or \\[ede-new] on
a directory that is not listed, Emacs will offer to add it to the
list.

Any other value disables searching for Org collection files."
  :group 'org-collection
  :type '(choice (const :tag "Any directory" t)
		 (repeat :tag "List of directories"
			 (directory))
		 (function :tag "Predicate"))
  :risky t)

(defun org-collection-directory-safe-p (dir)
  "Return non-nil if DIR is a safe directory to load.
Project types, such as those created with Project.ede files, are
safe only if specified by `ede-project-directories'."
  (setq dir (directory-file-name (expand-file-name dir)))
  ;; Load only if allowed by `ede-project-directories'.
  (or (eq ede-project-directories t)
      (and (functionp ede-project-directories)
	   (funcall ede-project-directories dir))
      (and (listp ede-project-directories)
	   (member dir ede-project-directories))))

(defcustom org-collection-default-directory t
  "Reset `org-directory' when switching between collections.
Reset `org-directory' to the path of the active Org collection
when switching collections. If nil then do not switch."
  :group 'org-collection
  :type 'boolean)

(defcustom org-collection-default-id-locations-file ".org-id-locations"
  "Default customization of `org-id-locations-file' within a collection.

The path is relative to the location of the collection unless
declared as an absolut path.

 If nil then `org-id-locations-file' is not implicitly modified
when a collection is active."
  :group 'org-collection
  :type 'file)

;;; Management variables

(defvar org-collections nil
  "A list of all active collections currently loaded in Emacs.")

(defvar org-collection-root nil
  "The current buffer's current root collection.
If a file is a part of a collection, this specifies the
collection that is at the root of the collection tree.")
(make-variable-buffer-local 'org-collection-root)

(defvar org-collection nil
  "The current buffer's current collection at that level.
If a file is a part of a collection, this specifies the
collection that contains the current file.")
(make-variable-buffer-local 'org-collection)

;;; Keymaps

(defvar org-collection-minor-mode-map
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "a" 'org-collection-agenda)
    ;; bind our submap into map
    (define-key map "\C-c." pmap)
    map)
  "Keymap used in project minor mode.")

;(defvar global-ede-mode-map
;  (let ((map (make-sparse-keymap)))
;    (define-key map [menu-bar cedet-menu]
;      (cons "Development" cedet-menu-map))
;    map)
;  "Keymap used in `global-ede-mode'.")

;;; Mode declarations

(defun org-apply-target-options ()
  "Apply options to the current buffer for the active collection."
  (org-collection-apply-local-variables))

(defun org-collection-turn-on-hook
    (if (and (stringp (buffer-file-name))
	     (stringp default-directory))
        (org-collection-mode)))

(define-minor-mode org-collection-mode
  :group 'org
  ())

(defun org-collection-initialize-state-current-buffer ()
  "Initialize the current buffer's state for org collection.
Sets buffer local variables for org collection."
  ;; Init the buffer.
  (let* ((ROOT nil)
         (proj (org-collection-get-open-project default-directory
                                                'ROOT)))
    (when (not proj)
      (warn "Not implemented yet"))
    (when proj
      (warn "Not implemented yet"))))

(defun org-collection-reset-all-buffers ()
  "Reset all the buffers due to change in org-collection."
  (interactive)
  (dolist (b (buffer-list))
    (when (buffer-file-name b)
      (with-current-buffer b
        ;; Reset all state variables
        (setq org-collection nil
              org-collection-root nil)
        ;; Now re-initialize this buffer.
        (org-collection-initialize-state-current-buffer)))))

;;;###autoload
(define-minor-mode global-org-collection-mode
  "Toggle global org collection mode.

This global minor mode enables `org-collection-mode' in all buffers
in an Org collection."
  :global t
  :group 'org
  (if org-collection-mode
      ;; Turn on org-collection-mode
      (progn
	(add-hook 'find-file-hook 'org-collection-turn-on-hook t)
	(add-hook 'dired-mode-hook 'org-collection-turn-on-hook)
	;(add-hook 'kill-emacs-hook 'ede-save-cache)
	;(ede-load-cache)
	(org-collection-reset-all-buffers))
    ;; Turn off org-collection-mode
    (remove-hook 'find-file-hook 'org-collection-turn-on-hook)
    (remove-hook 'dired-mode-hook 'org-collection-turn-on-hook)
    ;(remove-hook 'kill-emacs-hook 'org-collection-save-cache)
    ;(ede-save-cache)
    (org-collection-reset-all-buffers)))

;;; Functions working with collections


;; TODO Needs refactoring since the library should turn on the
;; collection automatically based on `default-directory'.
;;;###autoload
(defun org-collection-set (collection)
  "Set active Org collection.

Lalala."
  (interactive (list (org-completing-read "Set collection: " org-collection-directories)))
  (let ((dir (cdr (assoc collection org-collection-directories))))
    (when org-collection-default-directory
      (setq org-directory dir))
    (when org-collection-default-id-locations-file
      (setq org-id-locations-file
            (expand-file-name org-collection-default-id-locations-file dir)))
    (let ((properties (org-collection--get-properties dir)))
      (org-collection--set-properties properties))
    (setq org-collection-active collection)))

;; TODO Needs refactoring since the library should turn on (and off!?)
;; the collection automatically based on `default-directory'.
;;;###autoload
(defun org-collection-unset (collection)
  "Unset Org collection custiomization.

This will reset Org mode customizations to the default, as
set after emacs was started."
  (interactive (list (org-completing-read "Set collection: " org-collection-directories)))
  (let ((dir (cdr (assoc collection org-collection-directories))))
    (setq org-directory
          (eval (car (get 'org-directory 'standard-value))))
    (setq org-id-locations-file
          (eval (car (get 'org-id-locations-file 'standard-value))))
    (let ((properties (org-collection--get-properties dir)))
      (org-collection--unset-properties properties))
    (setq org-collection-active nil)))

(defun org-collection--get-properties (directory)
  ""
  (let ((file (expand-file-name org-collection-file directory))
        properties)
    (when (file-readable-p file)
      (with-demoted-errors "Error reading org-collection config: %S"
        (with-temp-buffer
          (insert-file-contents file)
          (condition-case-unless-debug nil
              (let ((read-circle nil))
                (setq properties (read (current-buffer))))
            (end-of-file nil)))))))

(defun org-collection--set-properties (property-alist)
  ""
  (dolist (property property-alist)
    (let* ((symbol (car property))
           (value (cadr property))
           (name (symbol-name symbol)))
      (if (and (or (string-prefix-p "org-" name)
                   (string-prefix-p "ol-" name)
                   (string-prefix-p "ox-" name))
               (custom-variable-p symbol))
          (set symbol value)
        (warn "Invalid customization in Org collection: %s" name)))))

(defun org-collection--unset-properties (property-alist)
  ""
  (dolist (property property-alist)
    (let* ((symbol (car property))
           (value (cadr property))
           (name (symbol-name symbol)))
      (if (and (or (string-prefix-p "org-" name)
                   (string-prefix-p "ol-" name)
                   (string-prefix-p "ox-" name))
               (custom-variable-p symbol))
          (set symbol
               (eval (car (get symbol 'standard-value))))))))

(defun project-try-org (dir)
  (let ((project-dir
         t))
    (when project-dir
      (t))))

(add-hook 'project-find-functions #'project-try-org)

(provide 'org-collection)

;;; org-collection.el ends here

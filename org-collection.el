;;; org-collection.el -*- lexical-binding: t; -*-

;;; Code:

(require 'org)

;;;; Customizations

(defconst org-collection-file ".org-collection"
  "Filename for a collection.

To reside in the directory root of a collection.")

(defgroup org-collection nil
  "Options concerning configuring Org collections."
  :tag "Org Collection"
  :group 'org)

(defcustom org-collection-list-file
  (locate-user-emacs-file "org-collections")
  "File in which to save the list of known collections."
  :type 'file)

(defcustom org-collection-directories t
  "Directories in which Org collection may search for files.
If the value is t, Org collection may search in any directory.

If the value is a function, Org collection calls that function
with one argument, the directory name; the function should return
t if Org collection should look for files in the directory.

Otherwise, the value should be a list of fully-expanded directory
names. Org collection searches for files only in those
directories.

If you invoke the commands \\[ede] or \\[ede-new] on a directory
that is not listed, Emacs will offer to add it to
`org-collection-list-file'.

Any other value disables searching for Org collection files."
  :group 'org-collection
  :type '(choice (const :tag "Any directory" t)
		 (repeat :tag "List of directories"
			 (directory))
		 (function :tag "Predicate")))

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

;;;; Functions working with collections

(defun org-collection-directory-p (dir)
  "Return non-nil if DIR is a safe directory to load.
Directories are okay to scan only if specified by
`org-collection-directories'."
  (setq dir (directory-file-name (expand-file-name dir)))
  ;; Scan only if allowed by `org-collection-directories'.
  (or (eq org-collection-directories t)
      (and (functionp org-collection-directories)
	   (funcall org-collection-directories dir))
      (and (listp org-collection-directories)
	   (member dir org-collection-directories))))

;;;###autoload
(defun org-collection-set (collection)
  "Set active Org collection."
  (let ((name (plist-get collection ':name))
        (location (plist-get collection ':location))
        (customization (plist-get collection ':customization)))
    (when org-collection-default-directory
      (setq-local org-directory default-directory))
    (when org-collection-default-id-locations-file
      (let ((id-file (expand-file-name org-collection-default-id-locations-file
                                       default-directory)))
        (org-collection--set-global-properties `((org-id-locations-file ,id-file)))
        (setq-local org-id-locations-file id-file)))
    (org-collection--set-local-properties customization)
    (org-collection--set-global-properties customization)
    (org-id-locations-load)
    (setq-local org-id-track-globally t)
    (setq-local org-collection-active collection)
    (setq org-collection collection)))

;;;###autoload
(defun org-collection-unset (collection)
  "Unset Org collection custiomization.
This will reset Org mode customizations to the default, as
set after emacs was started."
  (interactive (list (org-completing-read "Set collection: " org-collection-directories)))
  (let ((customization (plist-get collection ':customization)))
    (kill-local-variable 'org-directory)
    (kill-local-variable 'org-id-locations-file)
    (org-collection--unset-local-properties customization)
    (org-collection--unset-global-properties)
    (org-id-locations-load)
    (kill-local-variable 'org-id-track-globally)
    (setq org-collection-active nil)
    (setq org-collection nil)))

(defun org-collection-apply-in-current-buffer (collection)
  "Apply options to the current buffer for the active collection."
  (org-collection-set collection))

(defun org-collection-try-enable ()
  "Enable the collection"
    (if (and (stringp (buffer-file-name))
	     (stringp default-directory))
        (when (and (not org-collection)
                   (org-collection-directory-p default-directory))
          (let ((collection (org-collection-get default-directory)))
            (when collection
              (org-collection-set collection)
              (org-collection-update-list collection))))))

(defun org-collection-try-disable ()
  "Disable the collection"
    (if (and (stringp (buffer-file-name))
	     (stringp default-directory))
        (when (and org-collection
                   (org-collection-directory-p default-directory))
          (let ((collection (org-collection-get default-directory)))
            (when collection (org-collection-unset collection))))))

(defun org-collection-maybe-load-list-file ()
  "If the collection list have not been loaded from file, load it."
  (when (and (not org-collection-list)
             (file-readable-p org-collection-list-file))
    (with-temp-buffer
      (insert-file-contents org-collection-list-file)
      (condition-case-unless-debug nil
          (let ((read-circle nil))
            (setq org-collection-list (read (current-buffer))))
        (end-of-file nil)))))

(defun org-collection-update-list (collection)
  (let* ((name (plist-get collection ':name))
         (location (plist-get collection ':location))
         (location-existing (lax-plist-get org-collection-list name)))
    (cond ((and location-existing
                (not (equal location-existing location)))
           ;; Might add some user-interaction here later...
           (setq org-collection-list
                 (plist-put org-collection-list name location))
           (org-collection--try-persist org-collection-list))
          ((not location-existing)
           (setq org-collection-list
                 (plist-put org-collection-list name location))
           (org-collection--try-persist org-collection-list)))))

(defun org-collection--try-persist (c-list)
  "Persist collection if possible.
If collection is not listed in `org-collection-list-file'
already, persist it there."
  (when org-collection-list-file
    (save-excursion
      (condition-case nil
          (progn
            (set-buffer (find-file-noselect org-collection-list-file t))
            (erase-buffer)
            (insert ";; Org collection list file.
;; This contains a plist of known org collections with collection
;; `:name' as key and filesystem location as value.\n")
            (pp c-list (current-buffer))
            (insert "\n")
            (condition-case nil
                (save-buffer 0)
              (error
	       (message "File %s could not be saved."
		        org-collection-list-file)))
            (kill-buffer (current-buffer)))
        (error
         (message "File %s could not be read." org-collection-list-file))))))

(defun org-collection--set-local-properties (property-alist)
  ""
  (dolist (property property-alist)
    (let* ((symbol (car property))
           (value (cadr property))
           (name (symbol-name symbol)))
      (if (and (or (string-prefix-p "org-" name)
                   (string-prefix-p "ol-" name)
                   (string-prefix-p "ox-" name))
               (custom-variable-p symbol))
          (set (make-local-variable symbol) value)
        (warn "Invalid customization in Org collection: %s" name)))))

(defun org-collection--unset-local-properties (property-alist)
  ""
  (dolist (property property-alist)
    (let* ((symbol (car property))
           (value (cadr property))
           (name (symbol-name symbol)))
      (if (and (or (string-prefix-p "org-" name)
                   (string-prefix-p "ol-" name)
                   (string-prefix-p "ox-" name))
               (custom-variable-p symbol))
          (kill-local-variable symbol)))))

(defun org-collection--set-global-properties (property-alist)
  "Set global values for org collection.
If a key in PROPERTY-ALIST match a predefined key within this
function, the value of that property is set globally for the key.

This is mostly discouraged but some things in the Org universe
(still) requires globals to work.  This should be considered a
hack until those things get support for org collections."
  (let ((allowed-globals '(org-agenda-files
                           org-todo-keywords
                           org-id-locations-file)))
    (dolist (property property-alist)
      (let* ((symbol (car property))
             (value (cadr property))
             (name (symbol-name symbol)))
        (when (and (member symbol allowed-globals)
                   (custom-variable-p symbol)
                   (not (equal (default-value symbol) value)))
          (setq org-collection-globals-plist
                (plist-put org-collection-globals-plist symbol (default-value symbol)))
          (set-default symbol value))))))

(defun org-collection--unset-global-properties ()
  "Reset global properties.
Goes through plist `org-collection-globals-plist' and (re)sets symbols
to their default value."
  (while org-collection-globals-plist
    (let ((symbol (pop org-collection-globals-plist))
          (value (pop org-collection-globals-plist)))
      (set symbol value))))

(defun org-collection-get (dir)
  "Return an Org collection given a directory."
  (let ((file (expand-file-name org-collection-file dir))
        collection)
    (when (file-readable-p file)
      (with-demoted-errors "Error reading org-collection config: %S"
        (with-temp-buffer
          (insert-file-contents file)
          (condition-case-unless-debug nil
              (let ((read-circle nil))
                (setq collection (cdr (read (current-buffer)))))
            (end-of-file nil)))))
    (when collection
      (plist-put collection :location file))))

(defun org-collection-reset-all-buffers ()
  "Reset all the buffers due to change in org-collection."
  (interactive)
  (dolist (b (buffer-list))
    (when (buffer-file-name b)
      (with-current-buffer b
        (if org-collection-mode
            (org-collection-try-enable)
          (org-collection-try-disable))))))

(defun org-collection-reset-variables ()
  "Resets global variables in org-collection.

Some variables are expected to be reset elsewhere. Notably
`org-collection-globals-plist' which is traversed and emptied in
`org-collection--unset-global-properties'."
  (setq org-collection nil
        org-collection-list nil
        org-collection-active nil))

;;;; Management variables

(defvar org-collection nil
  "The current buffer's collection at that level.
If a file is a part of a collection, this specifies the
collection that contains the current file.")
(make-variable-buffer-local 'org-collection)

(defvar org-collection-active nil
  "Variable used for globally determining the active org-collection.

Wen set, org-collection-active will be a collection object, as
returned from `org-collection-get'.")

(defvar org-collection-list nil
  "Variable used to cache the content of `org-collection-file'.
Used only to reduce filesystem access.")

(defvar org-collection-globals-plist nil
  "Plist of modified global variables and their defaults.
This variable is used internally to keep track of global changes
that has been made, to be able to reset them when the mode is
disabled.")

;;;; Keymaps

(defvar org-collection-mode-map
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "a" 'org-collection-agenda)
    ;; bind our submap into map
    (define-key map "\C-c." pmap)
    map)
  "Keymap used in project minor mode.")

;;;; Mode declarations

;;;###autoload
(define-minor-mode org-collection-mode
  "Comment."
  nil " OrgC" org-collection-mode-map
  :global t
  (cond (org-collection-mode
         ;; Mode was turned on
         (add-hook 'find-file-hook 'org-collection-try-enable)
	 (add-hook 'dired-mode-hook 'org-collection-try-enable)
         (org-collection-reset-all-buffers)
         (org-collection-maybe-load-list-file))
        (t
         ;; Mode was turned off (or we didn't turn it on)
         (remove-hook 'find-file-hook 'org-collection-try-enable)
         (remove-hook 'dired-mode-hook 'org-collection-try-enable)
         (org-collection-reset-all-buffers)
         (org-collection-reset-variables))))

;;;; Connect to emacs project and module provisioning

(add-hook 'project-find-functions #'org-collection-get)

(provide 'org-collection)

;;; org-collection.el ends here

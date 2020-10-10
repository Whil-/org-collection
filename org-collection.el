;;; org-collection.el --- Less global Org collections -*- lexical-binding: t; sentence-end-double-space: t -*-

;; Copyright © 2020 Gustav Wikström <gustav@whil.se>

;; Author: Gustav Wikström <gustav@whil.se>
;; URL: https://github.com/whil-/org-collection
;; Keywords: org-mode, collection, project
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library aims at making Org mode things local to folders and
;; collections of Org mode documents.  It provides a global-minor-mode
;; `org-collection-mode' that tries, with the use of hooks and
;; advices, to load custom Org mode settings defined in
;; `org-collection-file'.  Switching between collections is automatic
;; and based on `default-directory'.  To simplify jumping between
;; collection an auxiliary function is also provided:
;; `org-collection-goto'.
;;
;;; Code:
;;;; Dependencies
(require 'org)

;;;; Customizations

(defconst org-collection-file ".org-collection"
  "Filename for a collection.

To reside in the directory root of a collection.")

(defgroup org-collection nil
  "Options concerning configuring Org collection."
  :tag "Org Collection"
  :group 'org)

(defcustom org-collection-list-file (locate-user-emacs-file "org-collections")
  "File in which to save the list of known collections."
  :type 'file)

(defcustom org-collection-directories t
  "Directories in which Org collection may search for files.
If the value is t, Org collection may search in any directory.

If the value is a function, Org collection calls that function
with one argument, the directory name; the function should return
t if Org collection should look for files in the directory.

Otherwise, the value should be a list of fully-expanded directory
names.  Org collection searches for files only in those
directories.

Any other value disables searching for Org collection files."
  :group 'org-collection
  :type '(choice (const :tag "Any directory" t)
		 (repeat :tag "List of directories"
			 (directory))
		 (function :tag "Predicate")))

(defcustom org-collection-default-id-locations-file ".org-id-locations"
  "Default customization of `org-id-locations-file' within a collection.

The path is relative to the location of the collection unless
declared as an absolut path."
  :group 'org-collection
  :type 'file)

;(defcustom org-collection-default-id-locations-file-relative t
;  "Override value of `org-id-locations-file-relative' for Org collection.
;For collections to be portable everything should be relative to
;the collection path by default.  This customization sets that
;default.  If set to nil, use value from
;`org-id-locations-file-relative' instead."
;  :group 'org-collection
;  :type 'boolean)

(defcustom org-collection-variables '((":path:" . (lambda (dir) dir)))
  "List of variables to be used in an org-collection-file.
The variables are literal values that will be replaced with the
output from the associated function before the configuration is
read and evaluated.

This is configured using an alist with keys as the literals to
replace and functions for determining the value that is to be put
instead.  The functions will be called with one argument: the
directory of the collection."
  :group 'org-collection
  :type '(alist :key-type string :value-type: function
                :tag "Alist of variable and function"))

;;;; Management variables

(defvar-local org-collection-buffer-cached nil
  "Variable for keeping track of buffer scans.
If this is set to anything but nil that means the minor mode
should stop all further processing.

This is intentional buffer local so that Org collection can keep
track of buffers one by one.")
;; Org-collection-local is declared as permanent so that
;; mode-switching or mode-reloading doesn't invalidate the variable.
;;;###autoload
(put 'org-collection-buffer-cached 'permanent-local t)

(defvar-local org-collection-local nil
  "The current buffer's collection at that level.
If a file is a part of a collection, this specifies the
collection that contains the current file.

This is intentional buffer local to help Org collection keeping
track of buffers one by one.")
;; Org-collection-local is declared as permanent so that
;; mode-switching doesn't invalidate the variable.
;;;###autoload
(put 'org-collection-local 'permanent-local t)

(defvar org-collection-global nil
  "Variable used for globally determining the active org-collection.

Wen set, `org-collection-global' will be a collection object, as
returned from `org-collection--try-get-collection'.")

(defvar org-collection-lock nil
  "Variable used for determining if there is an active lock.")

(defvar org-collection-list nil
  "Variable used to cache the content of `org-collection-file'.
Used only to reduce filesystem access.")

(defvar org-collection-global-defaults-plist nil
  "Plist of modified global variables and their defaults.
This variable is used internally to keep track of global changes
that has been made, to be able to reset them when the mode is
disabled.")

(defvar org-collection-debug nil
  "... Used for debugging purposes...")

;;;; Functions working with collections
;;;;; Internals
(defun org-collection--directory-p (dir)
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

(defun org-collection--set-global (collection &optional full)
  "Configure global customization for active Org collection.
Set some Org mode properties globally for a COLLECTION, for it
to work well with the rest of Org mode, for things not relying on
buffer local configurations.

With optional argument FULL, all customizations in a collection
is configured globally."
  (let* ((location (plist-get collection ':location))
         (customization-alist (plist-get collection ':customization))
         (require-alist (plist-get collection ':require))
         (id-file (expand-file-name org-collection-default-id-locations-file
                                    location)))
    (org-collection--set-global-variables `((org-directory ,location)
                                            (org-id-locations-file ,id-file)
                                            (org-id-track-globally t)
                                            (org-id-locations-file-relative t)))
    (dolist (package require-alist)
      (condition-case nil
          (unless (featurep package)
            (require package))
        (file-missing (warn "Could not load %s" (symbol-name package)))))
    (org-collection--set-global-variables customization-alist full)
    (org-id-locations-load)
    (setq org-collection-global collection)))

(defun org-collection--set-local (buffer collection)
  "Configure buffer local customization for active Org collection.
"
  (let ((customization-alist (plist-get collection ':customization)))
    (org-collection--set-local-variables buffer customization-alist)
    (with-current-buffer buffer
      (setq-local org-collection-local collection))))

(defun org-collection--unset-global ()
  "Unset Org collection custiomization.
This will reset global Org mode customizations to the default, as
set after emacs was started."
  (org-collection--unset-global-variables)
  (org-id-locations-load)
  (setq org-collection-global nil))

(defun org-collection--unset-local (buffer)
  "Unset Org collection custiomization.
This will reset buffer local Org mode customizations to the
default, as set after emacs was started."
  (org-collection--unset-local-variables buffer)
  (with-current-buffer buffer
    (setq org-collection-local nil
          org-collection-buffer-cached nil)))

(defun org-collection--set-local-variables (buffer property-alist)
  ""
  (with-current-buffer buffer
   (dolist (property property-alist)
     (let* ((symbol (car property))
            (value (cadr property))
            (name (symbol-name symbol)))
       (cond ((and (or (string-prefix-p "org-" name)
                        (string-prefix-p "ol-" name)
                        (string-prefix-p "ox-" name))
                   (custom-variable-p symbol))
              (set (make-local-variable symbol) value)
              (put symbol 'permanent-local t))
             (t
              (warn "Invalid customization in Org collection: %s" name)))))))

(defun org-collection--set-global-variables (property-alist &optional force)
  "Set global values for org collection.
If a key in PROPERTY-ALIST match a predefined key within this
function, the value of that property is set globally for the key.

This is mostly discouraged but some things in the Org universe
(still) requires globals to work.  This should be considered a
hack until those things get support for org collection.

With optional argument FORCE, all properties are set, even if
they're outside of the list of allowed globals."
  (let ((allowed-globals '(org-directory
                           org-agenda-files
                           org-todo-keywords
                           org-id-locations-file
                           org-id-track-globally
                           org-id-locations-file-relative)))
    (dolist (property property-alist)
      (let* ((symbol (car property))
             (value (cadr property))
             (name (symbol-name symbol)))
        (when (and (or force
                       (member symbol allowed-globals))
                   (custom-variable-p symbol)
                   (not (equal (default-value symbol) value)))
          (setq org-collection-global-defaults-plist
                (plist-put org-collection-global-defaults-plist symbol (default-value symbol)))
          (set-default symbol value))))))

(defun org-collection--unset-local-variables (buffer)
  ""
  (with-current-buffer buffer
    (let ((customization-alist (plist-get org-collection-local ':customization)))
      (dolist (customization customization-alist)
        (let* ((symbol (car customization))
               (value (cadr customization))
               (name (symbol-name symbol)))
          (when (and (or (string-prefix-p "org-" name)
                       (string-prefix-p "ol-" name)
                       (string-prefix-p "ox-" name))
                   (custom-variable-p symbol))
            (put symbol 'permanent-local nil)
            (kill-local-variable symbol)))))))

(defun org-collection--unset-global-variables ()
  "Reset global properties.
Goes through plist `org-collection-global-defaults-plist' and (re)sets symbols
to their default value."
  (while org-collection-global-defaults-plist
    (let ((symbol (pop org-collection-global-defaults-plist))
          (value (pop org-collection-global-defaults-plist)))
      (set-default symbol value))))

(defun org-collection--try-load-list-file ()
  "If the collection list have not been loaded from file, load it."
  (when (and (not org-collection-list)
             (file-readable-p org-collection-list-file))
    (with-temp-buffer
      (insert-file-contents org-collection-list-file)
      (condition-case-unless-debug nil
          (let ((read-circle nil))
            (setq org-collection-list (read (current-buffer))))
        (end-of-file nil)))))

(defun org-collection--maybe-update-list (collection)
  ""
  (let* ((name (plist-get collection ':name))
         (location (plist-get collection ':location))
         (location-existing (lax-plist-get org-collection-list name)))
    (cond ((and (stringp location-existing)
                (stringp location)
                (not (equal (expand-file-name location-existing)
                            (expand-file-name location))))
           ;; Might add some user-interaction here later... Right now
           ;; just overwrite existing with new.
           (setq org-collection-list
                 (lax-plist-put org-collection-list name location))
           (org-collection--try-persist org-collection-list))
          ((not location-existing)
           (setq org-collection-list
                 (lax-plist-put org-collection-list name location))
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

(defun org-collection--try-get-collection (dir)
  "Return an Org collection given a directory, if it exists and works.
If `org-collection-global' already is set this is returned if the
location of that collection matches `default-directory'"
  (let ((file (expand-file-name org-collection-file dir))
        (org-collection-global-location (plist-get org-collection-global ':location))
        collection)
    (cond ((and org-collection-global-location
                (equal (expand-file-name org-collection-global-location)
                       (expand-file-name default-directory)))
           org-collection-global)
          ((file-readable-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (dolist (config org-collection-variables)
               (let ((from-str (car config))
                     (to-str (funcall (cdr config) dir)))
                 (save-excursion
                   (while (search-forward from-str nil t)
                     (replace-match to-str)))))
             (condition-case-unless-debug nil
                 (let ((read-circle nil))
                   (setq collection (cdr (read (current-buffer)))))
               (end-of-file nil)))
           (when collection
             (plist-put collection :location dir))))))

(defun org-collection--unset ()
  "Unset mode.
Resets buffers and global variables in org-collection.

Some variables are expected to be reset elsewhere.  Notably
`org-collection-global-defaults-plist' which is traversed and
emptied in `org-collection--unset-global-variables'."
  ;; Reset global state
  (org-collection--unset-global)
  ;; Reset buffer local states
  (dolist (b (buffer-list))
    (org-collection--unset-local b)
    (with-current-buffer b
      (when (eq major-mode 'org-mode)
        (let ((inhibit-message t))
          (org-mode-restart))))))

;;;;; Important stuff

(defun org-collection-check-buffer-function (&optional window)
  "Check current buffer and enable or disable a collection if needed.
This function works with the current buffer and is responsible
for figuring out what collection to use, if any.  It does so in
two phases, one is to configure the global state of Org required
for the collection.  The second phase is to configure the local
state on the active buffer.

This function tries to do as little as possible since it's
supposed to be used in a hook that triggers quite often!  Maybe
more conditional logic than needed due to that.  But in this
case (and so far) better safe than sorry."
  (when (and (not (minibufferp))
             (or (not org-collection-buffer-cached)
                 ;; Even if cached, enabling might still be needed if the
                 ;; local buffer collection is set but different the
                 ;; global.
                 (and org-collection-local
                      (not (eq org-collection-local
                               org-collection-global)))
                 ;; *Experimental* Make sure Org collection doesn't pollute
                 ;; the global Org mode settings in Org mode buffers that
                 ;; don't belong to a collection.
                 (and (eq major-mode 'org-mode)
                      org-collection-global
                      (not org-collection-local))))
    ;; Wrap the whole within an unwind-protect which disables the
    ;; hook, to prevent infinite loops
    (let ((advice1-enabled (advice-member-p 'org-collection-check-buffer-function 'org-mode))
          (hook1-enabled (memq 'org-collection-check-buffer-function window-selection-change-functions))
          (hook2-enabled (memq 'org-collection-check-buffer-function find-file-hook)))
      (unwind-protect
          (progn
            (when advice1-enabled (advice-remove #'org-mode #'org-collection-check-buffer-function))
            (when hook1-enabled (remove-hook 'window-selection-change-functions
                                             #'org-collection-check-buffer-function))
            (when hook2-enabled (remove-hook 'find-file-hook
                                             #'org-collection-check-buffer-function))
            ;; Do some more checks to see if a collection really should be enabled
            (when (and (stringp default-directory)
                       (org-collection--directory-p default-directory))
              (let ((collection (or org-collection-local
                                    (org-collection--try-get-collection default-directory))))
                (when collection
                  ;; Deal with global collection settings.  Only
                  ;; configure it if it's different from an already
                  ;; set global config.
                  (unless (eq collection org-collection-global)
                    ;; If there is an existing setting, scratch that
                    ;; one first to not leave any residue.
                    (when org-collection-global
                      (org-collection--unset-global))
                    (org-collection--set-global collection)
                    (org-collection--maybe-update-list collection))
                  ;; Deal with buffer local collection settings.
                  (when (not (eq collection org-collection-local))
                    ;; If a local collection exist for the buffer
                    ;; since before, unset it to make sure no unwanted
                    ;; configs are left.
                    (when org-collection-local
                      (org-collection--unset-local (current-buffer) org-collection-local))
                    (org-collection--set-local (current-buffer) collection)
                    ;; Org mode needs a refresh if the collection was
                    ;; found and in need of update.  Inhibit messages
                    ;; from Org mode when doing this.  This is
                    ;; unfortunately needed since the buffer local
                    ;; config might be different after Org collection
                    ;; has been set.
                    (when (eq major-mode 'org-mode)
                      (let ((inhibit-message t))
                        (org-mode-restart)))))))
            ;; If major mode is Org and local settings are not set,
            ;; make sure to also unset global settings if they happen
            ;; to be set.
            (when (and (eq major-mode 'org-mode)
                       org-collection-global
                       (not org-collection-local))
              (org-collection--unset-global))
            (org-collection-update-mode-line)
            (setq org-collection-buffer-cached t))
        (when advice1-enabled (advice-add #'org-mode :before #'org-collection-check-buffer-function))
        (when hook1-enabled (add-hook 'window-selection-change-functions
                                      #'org-collection-check-buffer-function))
        (when hook2-enabled (add-hook 'find-file-hook
                                      #'org-collection-check-buffer-function))))))

;;;;; Interactive stuff

(defun org-collection-refresh ()
  "Refresh the configuration of the collection"
  (interactive)
  (org-collection--unset)
  (org-collection-check-buffer-function))

(defun org-collection-goto (collection-name)
  "Goto a collection."
  (interactive (list (org-completing-read "Goto collection: " (map-keys org-collection-list))))
  (let ((dir (lax-plist-get org-collection-list collection-name)))
    (find-file dir)))

(defun org-collection-lock (collection-name)
  "Enforces customizations for a collection to always be active."
  (interactive (list (org-completing-read "Lock collection: " (map-keys org-collection-list))))
  (if (not org-collection-mode)
      (message "Cannot lock a collection unless org-collection-mode is turned on.")
    (when org-collection-lock
      (org-collection-unlock))
    (let* ((dir (lax-plist-get org-collection-list collection-name))
           (collection (org-collection--try-get-collection dir)))
      ;; Unset all hooks and advices, since a lock is in place for the collection
      (advice-remove #'org-mode #'org-collection-check-buffer-function)
      (remove-hook 'window-selection-change-functions #'org-collection-check-buffer-function)
      (remove-hook 'find-file-hook #'org-collection-check-buffer-function)

      ;; Unset existing collection and buffer local configurations
      (org-collection--unset)

      ;; Enforce the whole collection to be global
      (org-collection--set-global collection t)

      ;; Mark the lock in the mode-line
      (org-collection-update-mode-line t)
      (setq org-collection-lock t))))

(defun org-collection-unlock ()
  "Remove potential lock and enable events"
  (interactive)
  (if (not org-collection-mode)
      (message "Org collection mode is not active, nothing to unlock.")

    ;; Unset existing globals
    (org-collection--unset-global)

    ;; Enable hooks and advices again
    (advice-add #'org-mode :before #'org-collection-check-buffer-function)
    (add-hook 'find-file-hook #'org-collection-check-buffer-function)
    (add-hook 'window-selection-change-functions #'org-collection-check-buffer-function)

    ;; Force a check of the active buffer
    (org-collection-check-buffer-function)
    (setq org-collection-lock nil)))

;;;; Keymaps

(defvar org-collection-mode-map
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "z" 'org-collection-mode)
    ;; bind our submap into map
    (define-key map "\C-c" pmap)
    map)
  "Keymap used in org collection global minor mode.")

;;;; Mode line stuff

(defcustom org-collection--mode-line-prefix " OC"
  "Mode line lighter prefix for org collection."
  :group 'org-collection
  :type 'string)

(defvar-local org-collection--mode-line org-collection--mode-line-prefix
  "String displayed in the mode line when Org collection global
  mode is turned on.")
(put 'org-collection--mode-line 'permanent-local t)

(defun org-collection-mode-line (collection &optional lock)
  "Report collection name in the modeline."
  (let* ((name (plist-get collection ':name)))
    (format "%s%s%s"
            org-collection--mode-line-prefix
            (if lock "!" "")
            (if name (format ":%s" name) ""))))

(defun org-collection-update-mode-line (&optional lock)
  "Set `org-collection--mode-line'.
The value is set in the current buffer, which should be a buffer
that belongs to the COLLECETION.

Uses buffer-local variable `org-collection-global' to determine
how the mode-line shall look."
  (let ((mode-line (org-collection-mode-line org-collection-global lock)))
    (cond (lock
           ;; With a lock the modeline should be locked in all
           ;; buffers. Since `org-collection--mode-line' is buffer
           ;; local this requires iteration.
           (dolist (b (buffer-list))
             (with-current-buffer b
               (setq org-collection--mode-line mode-line)))
           ;; Also set the default value, but make sure it's reset
           ;; when unlocked or the mode is disabled.
           (setq org-collection-global-defaults-plist
                 (plist-put org-collection-global-defaults-plist
                            'org-collection--mode-line
                            org-collection--mode-line-prefix))
           (setq-default org-collection--mode-line mode-line))
          (t
           (setq org-collection--mode-line mode-line)))
    (force-mode-line-update)
    mode-line))

;;;; Minor mode declarations
;;;###autoload
(define-minor-mode org-collection-mode
  "Comment."
  :init-value nil
  :lighter org-collection--mode-line
  :keymap org-collection-mode-map
  :global t
  :version "27.1"
  (cond (org-collection-mode
         ;; Mode was turned on.

         ;; Try to load the list file before enabling the event. Saves
         ;; one unwind-protect!
         (org-collection--try-load-list-file)
         (advice-add #'org-mode :before #'org-collection-check-buffer-function)
         (add-hook 'find-file-hook #'org-collection-check-buffer-function)
         (add-hook 'window-selection-change-functions #'org-collection-check-buffer-function)

         ;; Initial check
         (org-collection-check-buffer-function))
        (t
         ;; Mode was turned off (or we didn't turn it on)
         (advice-remove #'org-mode #'org-collection-check-buffer-function)
         (remove-hook 'find-file-hook #'org-collection-check-buffer-function)
         (remove-hook 'window-selection-change-functions #'org-collection-check-buffer-function)

         ;; Unset after hook is removed. Saves one unwind-protect!
         (org-collection--unset)
         (setq org-collection-list nil))))

;;;; Debugging

(defun org-collection-trace ( &optional text )
  (with-current-buffer (get-buffer-create "*OC Trace*")
    (goto-char (point-max))
    (if (stringp text)
        (insert (concat text "\n"))
      (insert "Tracing... Something happened\n"))))
(defun org-collection-trace-org-mode (&optional _) (org-collection-trace "Advice: org-mode"))
(defun org-collection-trace-find-file (&optional _) (org-collection-trace "Hook: find-file"))
(defun org-collection-trace-window (&optional _) (org-collection-trace "Hook: window-selection-change-functions"))

(defun org-collection-toggle-event-trace ()
  (interactive)
  (if (not  org-collection-debug)
      (progn (advice-add #'org-mode :before #'org-collection-trace-org-mode)
             (add-hook 'find-file-hook #'org-collection-trace-find-file)
             (add-hook 'window-selection-change-functions #'org-collection-trace-window)
             (setq org-collection-debug t))
    (advice-remove #'org-mode #'org-collection-trace-org-mode)
    (remove-hook 'find-file-hook #'org-collection-trace-find-file)
    (remove-hook 'window-selection-change-functions #'org-collection-trace-window)
    (setq org-collection-debug nil))
  (message (if org-collection-debug "Debug on" "Debug off")))

;;;; Connect to emacs project and module provisioning

(add-hook 'project-find-functions #'org-collection--try-get-collection)

(provide 'org-collection)

;;; org-collection.el ends here

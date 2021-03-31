;;; org-collection.el --- Less global Org setup -*- lexical-binding: t; sentence-end-double-space: t -*-

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
;; `org-collection-mode' that tracks which collection is active, if
;; any.  And it provides commands to switch between collection.
;; Switching between collections is manual and
;; `org-collection-register-and-lock' is the entry.  With registered
;; collections a more convenient function, `org-collection-lock', is
;; available.
;;
;; A bunch of auxiliary functions are provided as well to simplify
;; working with collections.  Like listing files and going to their
;; roots.
;;
;;; Code:
;;;; Dependencies
(require 'org)

;;;; (Global) Customizations

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

;;;; (Potentially local) Customizations

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

(defcustom org-collection-files-extension "org"
  "The extension for org files in a collection."
  :group 'org-collection
  :type '(string))

(defcustom org-collection-scan-directories-recursively t
  "If subdirectories inside `org-collection' are considered part of the collection or not."
  :group 'org-collection
  :type '(boolean))

;;;; Management variables

(defvar org-collection-global nil
  "Variable used for globally determining the active org-collection.

Wen set, `org-collection-global' will be a collection object, as
returned from `org-collection--try-get-collection'.")

(defvar org-collection-lock nil
  "Variable used for determining if there is an active lock.")

(defvar org-collection-list nil
  "Variable used to cache the content of `org-collection-file'.
Used mainly to reduce filesystem access.")

(defvar org-collection-global-defaults-plist nil
  "Plist of modified global variables and their defaults.
This variable is used internally to keep track of global changes
that has been made, to be able to reset them when the mode is
disabled.")

;;;; Functions working with collections
;;;;; Internals

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
    ;; Ignore the error message that org-id-locations-load may
    ;; display if a global org-id file is missing. It's noise at this location.
    (let ((inhibit-message t))
      (org-id-locations-load))
    (setq org-collection-global collection)))

(defun org-collection--unset-global ()
  "Unset Org collection custiomization.
This will reset global Org mode customizations to the default, as
set after emacs was started."
  (org-collection--unset-global-variables)
  ;; Ignore the error message that org-id-locations-load may
  ;; display if a global org-id file is missing. It's noise at this location.
  (let ((inhibit-message t))
    (org-id-locations-load))
  (setq org-collection-global nil))

(defun org-collection--set-global-variables (property-alist &optional force)
  "Set global values for org collection.
If a key in PROPERTY-ALIST match a predefined key within this
function, the value of that property is set globally for the key.

This is mostly discouraged but some things in the Org universe
(still) requires globals to work.  This should be considered a
hack until those things get support for org collection.

With optional argument FORCE, all properties are set, even if
they're outside of the list of allowed globals."
  (dolist (property property-alist)
    (let* ((symbol (car property))
           (value (cadr property))
           (name (symbol-name symbol)))
      (when (and (or force
                     (string-prefix-p "org-" name)
                     (string-prefix-p "ol-" name)
                     (string-prefix-p "ox-" name))
                 (custom-variable-p symbol)
                 (not (equal (default-value symbol) value)))
        (setq org-collection-global-defaults-plist
              (plist-put org-collection-global-defaults-plist symbol (default-value symbol)))
        (set-default symbol value)))))

(defun org-collection--unset-global-variables ()
  "Reset global properties.
Goes through plist `org-collection-global-defaults-plist' and (re)sets symbols
to their default value."
  (while org-collection-global-defaults-plist
    (let ((symbol (pop org-collection-global-defaults-plist))
          (value (pop org-collection-global-defaults-plist)))
      (set-default symbol value))))

(defun org-collection--try-load-list-file (&optional force)
  "If the collection list have not been loaded from file, load it.
When FORCE is non-nil reload file even if already loaded."
  (when (and (or force (not org-collection-list))
             (file-readable-p org-collection-list-file))
    (with-temp-buffer
      (insert-file-contents org-collection-list-file)
      (condition-case-unless-debug nil
          (let ((read-circle nil))
            (setq org-collection-list (read (current-buffer))))
        (end-of-file nil)))))

(defun org-collection--maybe-update-list (collection)
  "Update `org-collection-list' if needed."
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

(defun org-collection--lax-plist-delete (plist property)
  "Delete PROPERTY from PLIST with equal as comparison operator.
This is in contrast to merely setting it to 0.  Based on `org-plist-delete'."
  (let (p)
    (while plist
      (if (not (equal property (car plist)))
	  (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun org-collection--try-get-collection (dir)
  "Return an Org collection given a directory, if it exists and works.
If `org-collection-global' already is set this is returned if the
location of that collection matches `default-directory'.

If `org-collection-scan-directories-recursively' is not nil then
a collection can also be returned for paths closer to the root of
the filesystem-tree.  The deepest path takes precedence."
  (when-let* ((c-dir (if org-collection-scan-directories-recursively
                         (locate-dominating-file dir org-collection-file)
                       dir))
              (c-file (expand-file-name org-collection-file c-dir)))
    (let ((org-collection-global-location (plist-get org-collection-global ':location))
          collection)
      (cond ((and org-collection-global-location
                  (equal (expand-file-name org-collection-global-location)
                         (expand-file-name c-dir)))
             org-collection-global)
            ((file-readable-p c-file)
             (with-temp-buffer
               (insert-file-contents c-file)
               (dolist (config org-collection-variables)
                 (let ((from-str (car config))
                       (to-str (funcall (cdr config) c-dir)))
                   (save-excursion
                     (while (search-forward from-str nil t)
                       (replace-match to-str)))))
               (condition-case-unless-debug nil
                   (let ((read-circle nil))
                     (setq collection (cdr (read (current-buffer)))))
                 (end-of-file nil)))
             (when collection
               (plist-put collection :location c-dir)))))))

(defun org-collection--unset ()
  "Unset mode.
Resets global variables in org-collection.

Some variables are expected to be reset elsewhere.  Notably
`org-collection-global-defaults-plist' which is traversed and
emptied in `org-collection--unset-global-variables'."
  ;; Reset global state
  (org-collection--unset-global))

(defun org-collection-files (&optional relative collection)
  "Get all org files.
If RELATIVE is t, then return relative paths and remove file
extension.  Uses `org-global-collection' if `collection' is nil.
Ignores \"dotfiles\"."
  (let ((path (plist-get (or collection org-collection-global) ':location)))
    (if relative
        (mapcar #'org-collection-path-entry-name (org-collection-files))
      (if org-collection-scan-directories-recursively
          (directory-files-recursively
           path (format "^[^.].*\\.%s$" org-collection-files-extension))
        (directory-files
         path t (format "^[^.].*\\.%s$" org-collection-files-extension))))))

(defun org-collection-update-id-locations ()
  "Scan `org-collection-files' using `org-id-update-id-locations'."
  (interactive)
  (org-id-update-id-locations (org-collection-files)))

(defun org-collection-path-entry-name (path &optional collection)
  "Get PATH as an entry name."
  (let ((collection-path (plist-get (or collection org-collection-global) ':location)))
    (string-remove-suffix (concat "." org-collection-files-extension)
                          (file-relative-name (expand-file-name path)
                                              (expand-file-name collection-path)))))

;;;;; Interactive stuff

(defun org-collection-goto (collection-name)
  "Goto a collection."
  (interactive (list (org-completing-read "Goto collection: " (map-keys org-collection-list))))
  (let ((dir (lax-plist-get org-collection-list collection-name)))
    (find-file dir)))

(defun org-collection-visit-file (collection-file-no-extension)
  "Visit a file in a collection.
`collection-file-no-extension' is a path relative to the current
collection without file-extension."
  (interactive (list (org-completing-read "Visit: " (org-collection-files t))))
  (when-let* ((base-path (plist-get org-collection-global ':location))
              (file-fullname (expand-file-name (format "%s.%s" collection-file-no-extension
                                                       org-collection-files-extension)
                                               base-path)))
    (find-file file-fullname)))

(defun org-collection-register-and-lock (directory)
  "Opens a collection and registers it for easier future use."
  (interactive "Dorg-track directory: ")
  (let ((collection (org-collection--try-get-collection directory)))
    (cond (collection
           (org-collection--maybe-update-list collection)
           (org-collection-lock (plist-get collection ':name)))
          (t
           (error
            (message "No collection found at given location. Could not load or register"))))))

(defun org-collection-unregister (collection-name)
  "Removes a collection from the persisted list of collections."
  (interactive (progn
                 (org-collection--try-load-list-file 'force)
                 (list (org-completing-read "Unregister collection: " (map-keys org-collection-list)))))
  (setq org-collection-list
        (org-collection--lax-plist-delete org-collection-list collection-name))
  (org-collection--try-persist org-collection-list))

(defun org-collection-lock (collection-name)
  "Enforces customizations for a collection to always be active."
  (interactive (list (org-completing-read "Lock collection: " (map-keys org-collection-list))))
  (if (not org-collection-mode)
      (message "Cannot lock a collection unless org-collection-mode is turned on.")
    (when org-collection-lock
      (org-collection-unlock))
    (let* ((dir (lax-plist-get org-collection-list collection-name))
           (collection (org-collection--try-get-collection dir)))

      ;; Unset existing collection
      (org-collection--unset)

      ;; Enforce the whole collection to be global
      (org-collection--set-global collection)

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
    (setq org-collection-lock nil)
    (org-collection-update-mode-line)))

;;;; Keymaps

(defvar org-collection-mode-map
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "q" 'org-collection-mode)
    (define-key pmap "g" 'org-collection-goto)
    (define-key pmap "r" 'org-collection-register-and-lock)
    (define-key pmap "l" 'org-collection-lock)
    (define-key pmap "u" 'org-collection-unlock)
    ;; bind our submap into map
    (define-key map "\C-cz" pmap)
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
         (org-collection--try-load-list-file))
        (t
         ;; Unset after hook is removed. Saves one unwind-protect!
         (org-collection--unset)
         (setq org-collection-list nil))))

;;;; Connect to emacs project and module provisioning

(add-hook 'project-find-functions #'org-collection--try-get-collection)

(provide 'org-collection)

;;; org-collection.el ends here

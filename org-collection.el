;;; org-collection.el -*- lexical-binding: t; -*-

;;; Code:
;;;; Customization:
(require 'org)

(defvar org-collection-active nil
  "Variable used for determining the active org-collection.

org-collection-active should be either nil or one of the elements
in `org-collection-list'.")

(defgroup org-collection nil
  "Options concerning configuring Org collections."
  :tag "Org Collection"
  :group 'org)

(defcustom org-collection-list '(("Notes" . "~/Notes")
                                 ("Tasks" . "~/Tasks"))
  "List of alists for Org collection name keys and location of
  root for collection as value.

An Org collection implicitly sets the following variables:
- org-directory
- org-id-locations-file

The implicit defaults can be customized."
  :group 'org-collection
  :type '(alist :tag "Org collections"
                :key-type
                (string :tag "Name")
                :value-type
                (directory :tag "Path")))

(defcustom org-collection-file ".org-collection.el"
  ""
  :group 'org-collection
  :type 'string)

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

;;;; Functions

;;;###autoload
(defun org-collection-set (collection)
  "Set active Org collection.

Lalala."
  (interactive (list (org-completing-read "Set collection: " org-collection-list)))
  (let ((dir (cdr (assoc collection org-collection-list))))
    (when org-collection-default-directory
      (setq org-directory dir))
    (when org-collection-default-id-locations-file
      (setq org-id-locations-file
            (expand-file-name org-collection-default-id-locations-file dir)))
    (let ((properties (org-collection--get-properties dir)))
      (org-collection--set-properties properties))
    (setq org-collection-active collection)))

;;;###autoload
(defun org-collection-unset (collection)
  "Unset Org collection custiomization.

This will reset Org mode customizations to the default, as
set after emacs was started."
  (interactive (list (org-completing-read "Set collection: " org-collection-list)))
  (let ((dir (cdr (assoc collection org-collection-list))))
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

;;; End
(provide 'org-collection)

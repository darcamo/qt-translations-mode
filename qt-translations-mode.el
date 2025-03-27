;;; qt-translations-mode.el --- Major mode for working with Qt language files (.ts files).  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>
;; Keywords: Qt, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode that derives from nxml-mode and add a few niceties for
;; working with Qt language files, such as imenu support.

;;; Code:

(require 'nxml-mode)

(defcustom qt-translations-imenu-style 'message
  "Which imenu style to use when creating the entries.

Allowed values:
- `'context`: List all contexts.
- `'message`: List all messages.
- `'vanished`: List only vanished messages.
- `'unfinished`: List only unfinished messages.
- `'obsolete`: List only obsolete messages."
  :type '(choice (const :tag "List all contexts" context)
                 (const :tag "List all messages" message)
                 (const :tag "List vanished messages" vanished)
                 (const :tag "List unfinished messages" unfinished)
                 (const :tag "List obsolete messages" obsolete))
  :group 'qt-translations
  :safe (lambda (value)
          (memq value '(context message vanished unfinished obsolete))))


(defun qt-translations-mode-imenu-create-index-for-each-context ()
  "Create an imenu index for XML files with <context> elements.

Each imenu entry corresponds to the text inside the <name> child element
of a <context> element."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      ;; Search for <context> elements
      (while (re-search-forward "<context>\\([[:space:]\n]*\\)" nil t)
        (let ((context-start (match-beginning 0)))
          ;; Look for the <name> element inside the <context>
          (when (re-search-forward "<name>\\([[:space:]\n]*\\)\\([^<]+\\)\\([[:space:]\n]*\\)</name>" nil t)
            (let ((name (match-string 2))) ;; Extract the text inside <name>
              ;; Add the entry to the imenu index
              (push (cons name context-start) index))))))
    (nreverse index)))

(defun qt-translations-mode-imenu-create-index-for-each-message ()
  "Create an imenu index for XML files with <context> and <message> elements.

Each imenu entry corresponds to the text inside the <source> child
element of a <message>, prefixed by the <name> of the parent <context>."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      ;; Search for <context> elements
      (while (re-search-forward "<context>\\([[:space:]\n]*\\)" nil t)
        (let ((context-start (match-beginning 0)))
          ;; Look for the <name> element inside the <context>
          (when (re-search-forward "<name>\\([[:space:]\n]*\\)\\([^<]+\\)\\([[:space:]\n]*\\)</name>" nil t)
            (let ((context-name (match-string 2))
                  (sub-index nil)) ;; Submenu for messages in this context
              ;; Restrict the search for <message> elements to the current <context>
              (save-restriction
                (narrow-to-region context-start (save-excursion
                                                  (re-search-forward "</context>" nil t)))
                (goto-char context-start)
                (while (re-search-forward "<message>\\([[:space:]\n]*\\)" nil t)
                  (let ((message-start (match-beginning 0)))
                    ;; Look for the <source> element inside the <message>
                    (when (re-search-forward "<source>\\([[:space:]\n]*\\)\\([^<]+\\)\\([[:space:]\n]*\\)</source>" nil t)
                      (let ((source-text (match-string 2)))
                        ;; Add the entry to the sub-index
                        (push (cons source-text message-start) sub-index))))))
              ;; Add the context and its messages to the main index
              (when sub-index
                (push (cons context-name (nreverse sub-index)) index)))))))
    (nreverse index)))


(defun qt-translations-mode-imenu-create-index-for-each-message-with-type-attribute (type)
  "Create an imenu index for messages whose translation has type TYPE.

TYPE can be `'vanished', `'unfinished', or `'obsolete'."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      ;; Search for <message> elements
      (while (re-search-forward "<message>\\([[:space:]\n]*\\)" nil t)
        (let ((message-start (match-beginning 0)))
          ;; Look for the <translation> element inside the <message>
          (when (re-search-forward "<translation\\([^>]*\\)>\\([[:space:]\n]*\\)" nil t)
            (let ((attributes (match-string 1)))
              ;; Check if the translation has the specified type
              (when (and attributes
                         (string-match (format "type=\"%s\"" type) attributes))
                ;; Look for the <source> element inside the <message>
                (save-excursion
                  (goto-char message-start)
                  (when (re-search-forward "<source>\\([[:space:]\n]*\\)\\([^<]+\\)\\([[:space:]\n]*\\)</source>" nil t)
                    (let ((source-text (match-string 2)))
                      ;; Add the entry to the imenu index
                      (push (cons source-text message-start) index))))))))))
    (nreverse index)))

(defun qt-translations-mode-imenu-create-index-for-each-vanished-message ()
  "Create an imenu index for vanished messages."
  (qt-translations-mode-imenu-create-index-for-each-message-with-type-attribute "vanished"))

(defun qt-translations-mode-imenu-create-index-for-each-unfinished-message ()
  "Create an imenu index for unfinished messages."
  (qt-translations-mode-imenu-create-index-for-each-message-with-type-attribute "unfinished"))

(defun qt-translations-mode-imenu-create-index-for-each-obsolete-message ()
  "Create an imenu index for obsolete messages."
  (qt-translations-mode-imenu-create-index-for-each-message-with-type-attribute "obsolete"))


(defun qt-translations-mode-imenu-create-index ()
  "Create an imenu index for XML files corresponding to Qt language files.
The behavior depends on the value of `qt-translations-imenu-style`:
- `'context`: Group entries by context.
- `'message`: List all messages.
- `'vanished`: List only vanished messages.
- `'unfinished`: List only unfinished messages.
- `'obsolete`: List only obsolete messages."
  (cond
   ((eq qt-translations-imenu-style 'context)
    (qt-translations-mode-imenu-create-index-for-each-context))
   ((eq qt-translations-imenu-style 'message)
    (qt-translations-mode-imenu-create-index-for-each-message))
   ((eq qt-translations-imenu-style 'vanished)
    (qt-translations-mode-imenu-create-index-for-each-vanished-message))
   ((eq qt-translations-imenu-style 'unfinished)
    (qt-translations-mode-imenu-create-index-for-each-unfinished-message))
   ((eq qt-translations-imenu-style 'obsolete)
    (qt-translations-mode-imenu-create-index-for-each-obsolete-message))
   (t
    (error "Unknown value for `qt-translations-imenu-style`: %s" qt-translations-imenu-style))))


(define-derived-mode qt-translations-mode
  nxml-mode "qt-translations-mode" "A major-mode for editing Qt translation files."
  (setq nxml-heading-element-name-regexp "name\\|source")
  (setq nxml-section-element-name-regexp "context\\|message")

  (setq imenu-create-index-function #'qt-translations-mode-imenu-create-index)
  ;; (setq imenu-create-index-function #'my-xml-imenu-create-index-unfinished)
  )

;; NOTE: Emacs caches imenu entries. After changing the
;; `qt-translations-imenu-style` variable, just make any change in the
;; buffer to make emacs recompute the imenu entries.

(provide 'qt-translations-mode)
;;; qt-translations-mode.el ends here

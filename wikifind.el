;;; wikifind.el --- search wikipedia and return abbreviated results

;; Copyright (C) 2009  Ryan Yeske

;; Author: Ryan Yeske <rcyeske@gmail.com>
;; Keywords: convenience, languages, comm
;; Version: 0.51 20091023

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x wikifind RET searchterm

;; (global-set-key (kbd "C-c l") 'wikifind)

;;; Todo:

;;; Code:

(require 'json)
(require 'webjump) ; for webjump-url-encode

(defvar wikifind-default-language "en"
  "*Default language to use.")

(defvar wikifind-multiple-results-buffers nil
  "If non-nil, a new buffer is created for each unique query.")

(defvar wikifind-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "q") 'bury-buffer)
         map)
  "Keymap for `wikifind-mode'.")

(define-derived-mode wikifind-mode text-mode "wikifind"
  "Major mode for displaying `wikifind' results.
\\{wikifind-mode-map}"
  )

(defun wikifind (query-string)
  (interactive "sSearch wikipedia for: ")
  (let* ((query (assq 'query (wikifind-query query-string)))
         (totalhits (cdr (assq 'totalhits (cdr (assq 'searchinfo query)))))
         (hit-list (cdr (assq 'search query)))
         (inhibit-read-only t))
    (pop-to-buffer (get-buffer-create (wikifind-buffer-name query-string)))
    (erase-buffer)
    (wikifind-mode)
    (setq header-line-format (format "%s (%d)" query-string totalhits))
    (dolist (hit hit-list)
      (let ((title (cdr (assq 'title hit)))
            (snippet (wikifind-cleanup (cdr (assq 'snippet hit))))
            (start (point)))
        (insert-button title
                       'follow-link t
                       'face 'font-lock-function-name-face
                       'help-echo "browse to this page"
                       'action 'wikifind-button-title)
        (insert ": ")
        (insert-button snippet
                       'follow-link t
                       'face nil
                       'mouse-face nil
                       'help-echo "search for this word"
                       'action 'wikifind-button-snippet)
        (fill-region start (point))
        (newline 2)))
    (toggle-read-only t)
    (goto-char (point-min))))

(defun wikifind-query (query &optional lang)
  (let ((url (concat "http://"
                     (or lang wikifind-default-language)
                     ".wikipedia.org/w/api.php?"
                     "format=json"
                     "&action=query"
                     "&list=search"
                     "&srlimit=50"
                     "&srsearch=" (webjump-url-encode
                                   (concat "\"" query "\"")))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (prog1 
          (let ((json-array-type 'list))
            (json-read))
        (kill-buffer)))))

(defun wikifind-button-title (button)
  "Function called when clicking on page title text"
  (let ((page (buffer-substring (overlay-start button)
                                (overlay-end button))))
    (browse-url (concat "http://" wikifind-default-language ".wikipedia.org/wiki/"
                        page))))

(defun wikifind-button-snippet (button)
  "Function called when clicking on snippet text"
  (wikifind (word-at-point)))  

(defun wikifind-buffer-name (query)
  (if wikifind-multiple-results-buffers
      (concat "*wikifind: " query "*")
    "*wikifind*"))

(defun wikifind-cleanup (text)
  "Remove markup from TEXT."
  ;; bold text inbetween searchmatch spans.
  ;; replace-regexp-in-string doesnt let me replace with propertized text :(
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "<span class='searchmatch'>\\(.*?\\)</span>" nil t)
      (let ((replacement (buffer-substring (match-beginning 1)
                                           (match-end 1))))
        (delete-region (match-beginning 0) 
                       (match-end 0))
        (insert (propertize replacement 'face 'bold))))
    ;; remove any other markup
    (replace-regexp-in-string "<.*?>" ""
                              (buffer-substring (point-min) (point-max)))))

(provide 'wikifind)
;;; wikifind.el ends here

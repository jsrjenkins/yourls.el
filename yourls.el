;;; yourls.el --- URL Shortener with Yourls

;; Copyright (C) 2015  Yasushi SHOJI

;; Author: Yasushi SHOJI <yasushi.shoji@gmail.com>
;; URL: https://github.com/yashi/yourls-el
;; Package-Requires: ((request) (json "1.4"))
;; Keywords: convenience url-shortener

;;; Commentary:

;; This library implements convenient functions to shorten an url with
;; Yourls API, Your Own URL Shortener.
;;
;; It provides two commands to shorten, `yourls-at-point' and
;; `yourls-region'.
;;
;; To used it you have to define two variables, `yourls-api-endpoint'
;; and `yourls-api-signature'.  Your can learn them at
;; http://yourls.org/#API.


;;; Code:
(require 'request)
(require 'json)
(require 'thingatpt)

(defgroup yourls nil
  "Yourls URL shortener."
  :group 'convenience)

(defcustom yourls-api-endpoint nil
  "The API endpoint that you send your request to."
  :type 'string
  :group 'yourls)

(defcustom yourls-api-signature nil
  "Your signature for accessing the API."
  :type 'string
  :group 'yourls)

;;;###autoload
(defun yourls-at-point ()
  (interactive)
  (let ((url (thing-at-point 'url)))
    (when url
      (let* ((bounds (bounds-of-thing-at-point 'url))
             (beg (car bounds))
             (end (cdr bounds))
             (shortened (yourls-url (buffer-substring-no-properties beg end))))
        (when shortened
          (delete-region beg end)
          (insert shortened))))))

;;;###autoload
(defun yourls-region (arg)
  (interactive "P")
  (let* ((beg (region-beginning))
         (end (region-end))
         (url (yourls-url (buffer-substring-no-properties beg end))))
    (when url
      (delete-region beg end)
      (insert url))))

(defun yourls-url (url)
  (unless yourls-api-endpoint
    (user-error "`yourls-api-endpoint' is not defined."))
  (unless yourls-api-signature
    (user-error "`yourls-api-signature' is not defined."))
  (let (ret)
    (request
     yourls-api-endpoint
     :params `((signature . ,yourls-api-signature)
               (format . "json")
               (action . "shorturl")
               (url . ,url))
     :parser 'json-read
     :sync t
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (setq ret data))))
    (cdr (assoc 'shorturl ret))))

(provide 'yourls)

;;; yourls.el ends here

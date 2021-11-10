;;; yourls.el --- URL Shortener with Yourls

;; This is heavily modified from the original extension by Yasushi SHOJI,
;; which no longer is maintained and no longer works
;; to such an extent we publish under our own name.

;; Copyright (C) 2021  John Jenkisn

;; Author: Fr. John Jenkins <jenkins@sspx.org.za>
;; URL: https://github.com/jsrjenkins/yourls-el
;; Keywords: convenience url-shortener

;; YOURLS can be found here: https://github.com/YOURLS/YOURLS/releases

;;; Commentary:

;; This library implements convenient functions to shorten an url with
;; Yourls API, Your Own URL Shortener.
;;
;; It provides two commands to shorten, `yourls-at-point' and
;; `yourls-region' as well as `yourls-get-short' and `yours-custom' which
;; allows for a custom shortcut
;;
;; To used it you have to define two variables, `yourls-api-endpoint'
;; and `yourls-api-signature'.  Your can learn them at
;; http://yourls.org/#API.

;;; Code:
(defgroup yourls nil
  "Yourls URL shortener installation."
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
  "Shorten the URL at point with your YOURLS install"
    (interactive)
    (let ((url-bounds (bounds-of-thing-at-point 'url)))
      (when url-bounds
        (let ((url (yourls-get-short (thing-at-point 'url))))
          (when url
            (save-restriction
              (narrow-to-region (car url-bounds) (cdr url-bounds))
              (delete-region (point-min) (point-max))
              (insert url)))))))

;;;###autoload
(defun yourls-region (arg)
  (interactive "P")
  (let* ((beg (region-beginning))
         (end (region-end))
         (url (yourls-get-short (buffer-substring-no-properties beg end))))
    (when url
      (delete-region beg end)
      (insert url))))

(defun yourls-make-short (url)
  (unless yourls-api-endpoint
    (user-error "`yourls-api-endpoint' is not defined."))
  (unless yourls-api-signature
    (user-error "`yourls-api-signature' is not defined."))
  (let* ((querys (url-build-query-string `(("signature" ,yourls-signature)
                                           ("action" "shorturl")
                                           ("format" "json")
                                           ("url" ,url))))
         (final-url (concat yourls-url "/?" querys))
         (url-request-method "GET")
         (retrieved (url-retrieve-synchronously final-url)))
    (with-current-buffer retrieved
      (let* ((s (buffer-string))
             (shortened (when (string-match "\"shorturl\":\"\\([^\"]*\\)\""
                                            s)
                          (replace-regexp-in-string "\\\\" "" (match-string 1 s)))))
        shortened))))

(defun yourls-make-custom (url)
  (interactive "tTitle:")
  (unless yourls-api-endpoint
    (user-error "`yourls-api-endpoint' is not defined."))
  (unless yourls-api-signature
    (user-error "`yourls-api-signature' is not defined."))
  (let* ((querys (url-build-query-string `(("signature" ,yourls-signature)
                                           ("action" "shorturl")
					   ("keyword" ,t)
                                           ("format" "json")
                                           ("url" ,url))))
         (final-url (concat yourls-url "/?" querys))
         (url-request-method "GET")
         (retrieved (url-retrieve-synchronously final-url)))
    (with-current-buffer retrieved
      (let* ((s (buffer-string))
             (shortened (when (string-match "\"shorturl\":\"\\([^\"]*\\)\""
                                            s)
                          (replace-regexp-in-string "\\\\" "" (match-string 1 s)))))
        shortened))))

;; TODO : retrieve the original url from the shortened one 

(provide 'yourls)

;;; yourls.el ends here

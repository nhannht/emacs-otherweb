;;; otherweb.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 nhannht
;;
;; Author: nhannht <nhanclassroom@gmail.com>
;; Maintainer: nhannht <nhanclassroom@gmail.com>
;; Created: August 15, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nhannht/otherweb.el
;; Package-Requires: ((emacs "24.3"))
;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Create thoudsand of news with summary and NLP algorithm from otherweb.com
;;
;;; Code:

(require 'json)
(require 's)
(require 'dash)
(require 'url)
(defgroup otherweb nil
  "Otherweb group")
(defcustom otherweb--sort "recent"
  "3 type of sort recent, relevent, informative"
  :type '(choice (string :tag "recent")
                 (string :tag "relevant")
                 (string :tag "informative"))
  :group 'otherweb)

(defcustom otherweb--categories '(("politic"  50)
                                  ("tech"  50)
                                  ("world"  50)
                                  ("entertainment"  50)
                                  ("bussiness"  50)
                                  ("world"  50)
                                  ("entertainment"  50)
                                  ("science"  50)
                                  ("health"  50)
                                  ("sport"  50))
  "Higher point mean higher cut off (less number of news) in exchange to higher informative news.  Must be from 0 to 99"
  :type '(alist
          :key-type  (string :tag "Category")
          :value-type (group (restricted-sexp  :tag "Cutoff"
                                               :match-alternatives
                                               ((lambda (num)
                                                  (and
                                                   (numberp num)
                                                   (>= num 0)
                                                   (< num 100)))))))
  :group 'otherweb)

(defcustom otherweb--src '(("WSJmarkets" t)
                          ("FOXTV" t)
                          ("ScienceMagazine" t)
                          ("markets" t)
                          ("nytimesworld" t)
                          ("Jerusalem_Post" t)
                          ("ABCNetwork" t)
                          ("CBSPolitics" t)
                          ("foxnewspolitics" t)
                          ("FT" t)
                          ("NBCNewsTech" t)
                          ("BreakingNews" t)
                          ("nytopinion" t)
                          ("BBCBreaking" t)
                          ("cnnbrk" t)
                          ("gadgetlab" t)
                          ("accesshollywood" t)
                          ("BBCTech" t)
                          ("ftfinancenews" t)
                          ("macworld" t)
                          ("CNBCnow" t)
                          ("BBCScienceNews" t)
                          ("TelegraphNews" t)
                          ("WIREDBusiness" t)
                          ("applenws" t)
                          ("MTVNEWS" t)
                          ("WIREDScience" t)
                          ("PhysicsWorld" t)
                          ("BitcoinMagazine" t)
                          ("FinancialTimes" t)
                          ("NBCNewsHealth" t)
                          ("ScienceNews" t)
                          ("TIME" t)
                          ("guardiantech" t)
                          ("WritersDigest" t)
                          ("guardianscience" t)
                          ("theintercept" t)
                          ("bbchealth" t)
                          ("DiscoverMag" t)
                          ("TheHackersNews" t)
                          ("Breaking911" t)
                          ("NatGeo" t)
                          ("nytimes" t)
                          ("cnni" t)
                          ("WorldAndScience" t)
                          ("iPhone_News" t)
                          ("wmag" t)
                          ("NBCPolitics" t)
                          ("ABCPolitics" t)
                          ("FortuneMagazine" t)
                          ("VICENews" t)
                          ("MotherJones" t)
                          ("Gizmodo" t)
                          ("BuzzFeedNews" t)
                          ("ReutersPolitics" t)
                          ("CNN" t)
                          ("ReutersScience" t)
                          ("voguemagazine" t)
                          ("ReutersBiz" t)
                          ("VOANews" t)
                          ("Suntimes" t)
                          ("NewsHour" t)
                          ("WIRED" t)
                          ("SkyNewsAust" t)
                          ("HuffPostPol" t)
                          ("FOX4" t)
                          ("IndieWire" t)
                          ("CNETNews" t)
                          ("ABC" t)
                          ("thetimes" t)
                          ("AP" t)
                          ("chicagotribune" t)
                          ("YahooNews" t)
                          ("DailyMirror" t)
                          ("BBCNews" t)
                          ("thehill" t)
                          ("ABC7NY" t)
                          ("dallasnews" t)
                          ("CBSNews" t)
                          ("TMZ" t)
                          ("NBCNews" t)
                          ("mercnews" t)
                          ("Forbes" t)
                          ("BostonGlobe" t)
                          ("CNBC" t)
                          ("guardiannews" t)
                          ("SkyNews" t)
                          ("dcexaminer" t)
                          ("DailyMailUK" t)
                          ("guardian" t)
                          ("FoxNews" t)
                          ("Reuters" t))
  "Filter out news from those source with nil value"
  :type '(alist :value-type boolean)
  :group 'otherweb)


(defun otherweb--create-endpoint (&optional search_after)
  "Create endpoint from src,categories and sort type"
  (let ((o--src (cons "source" (mapcar #'car
                                       (-filter (lambda (x)
                                                  (cadr x))
                                                otherweb--src))))
        (o--categories (cons "category" (mapcar #'car otherweb--categories)))
        (o--cutoff (cons "cutoff" (mapcar (lambda (e)
                                            (format "%s:%s"
                                                    (car e)
                                                    (cadr e)))
                                          otherweb--categories)))
        (o--sort (cons "sort" (cons otherweb--sort nil))))
    (url-recreate-url (url-parse-make-urlobj "https"
                                             nil
                                             nil
                                             "s.valurank.com/api/v1/otherweb/news?"
                                             nil
                                             (url-build-query-string (list o--src
                                                                           o--categories
                                                                           o--cutoff
                                                                           o--sort
                                                                           (if search_after
                                                                               (list "search_after" search_after))))
                                             nil
                                             nil
                                             t))))

(defcustom otherweb--use-external-curl-p nil
  "Because i am newbie, i prefer using external curl command in linux"
  :type '(choice (const :tag "Use external curl, which is faster" t)
                 (const :tag "Use built-in elisp library" nil))
  :group 'otherweb)

(defun otherweb--get-data-builtin-library (&optional search_after)
  (let ((endpoint (if search_after
                      (otherweb--create-endpoint search_after)
                    (otherweb--create-endpoint))))
    (with-temp-buffer
      (url-insert-file-contents endpoint)
      ;; (goto-char url-http-end-of-headers)
      (json-read))))

(defun otherweb--get-data-using-curl (&optional search_after)
  "Get data using external curl."
  (let* (
         (endpoint (if search_after
                       (otherweb--create-endpoint search_after)
                    (otherweb--create-endpoint))))
    (json-read-from-string (shell-command-to-string (format "curl -s \"%s\"" endpoint)))))

(defun otherweb--getdata (&optional search_after)

  (if otherweb--use-external-curl-p
      (otherweb--get-data-using-curl (if search_after search_after))
    (otherweb--get-data-builtin-library (if search_after search_after))))

(defun otherweb--replace-summary-line (line)
  "Increase heading level of summary LINE ."
  (s-replace-regexp (rx  line-start "*") "- " line nil nil nil nil))

(defun otherweb--format-summary (summary)
  "Return the format version of SUMMARY."
  (s-join "\n" (-map #'otherweb--replace-summary-line  (s-split "\n" summary))))



(defun otherweb--parse-time-org-format (time-string)
  (format-time-string "<%Y-%m-%d %a %H:%M>"
                      (date-to-time time-string )))

(defun otherweb--parse-time-ago (time-string)
  (let ((s-ago (time-to-seconds (time-subtract (current-time) (date-to-time time-string )))))
    (cond ((< s-ago 60) (format "%d seconds ago" s-ago))
          ((< s-ago 3600) (format "%d minutes ago" (/ s-ago 60)))
          ((< s-ago 86400) (format "%d hours ago" (/ s-ago 3600)))
          ((< s-ago (* 86400 30)) (format "%d days ago" (/ s-ago 86400)))
          ( t(format "%d months ago" (/ s-ago (* 86400 30))))
          )))


(defun otherweb--format-item-org (item)
  "Input as json object from items vector and output each ITEM in org-format."
  (message "LOG:otherweb--format-item-org")
  (let* ((headline (cdr (assoc 'headline item)))
         (source_url (cdr (assoc 'source_url item)))
         (summary (cdr (assoc 'summary item)))
         (f-summary (otherweb--format-summary summary))
         (category (cdr (assoc 'category item)))
         (created_at (cdr (assoc 'created_at item)))
         (parse_created_at (otherweb--parse-time-org-format created_at))
         (ago (otherweb--parse-time-ago created_at))
         (source_username (cdr (assoc 'source_username item)))
         (source_name (cdr (assoc 'source_name item)))
         (informative (cdr (assoc 'informative item)))
         (id (cdr (assoc 'id item))))
    (format "
** %s  |[[%s][%s]]|%s|%.2f| :%s:%s:
:PROPERTIES:
:SOURCE_URL: [[%s]]
:CREATED_AT: %s
:INFORMATIVE: %s
:SOURCE_NAME: %s
:OTHERWEB_ID: %s
:END:
%s

" headline source_url source_name ago informative
category source_username source_url parse_created_at
informative source_name id f-summary)))

(defun otherweb--setup-header-line (total page sort)
  (setq header-line-format (format "%s results |Sort: %s | Page %s | Press C-M-right : next-page | C-M-left : previous page"
                                   total sort page) ))

(define-minor-mode otherweb-mode
  "Minor mode for otherweb"
  :lighter " otherweb"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-M-<right>") #'otherweb-next)
            (define-key map (kbd "C-M-<left>") #'otherweb-prev)
            map))
(defvar otherweb--data (list)
  "Store page.")
(defvar otherweb--current-pointer 0
  "Point to current page.")
(defun otherweb--point-to-last-item-p ()
  (and (> (length otherweb--data) 0)
       (= otherweb--current-pointer (1- (length otherweb--data)) ))
  )
(defun otherweb--clear-data ()
  "Clear all cache and set pointer to nil"
  (setq otherweb--data (list))
  (setq otherweb--current-pointer 0) )
(defun otherweb--inc-pointer ()
  "Increase with side effect"
  (let ((max  (1- (length otherweb--data))))
    (setq otherweb--current-pointer (if (>= otherweb--current-pointer max)
                                        otherweb--current-pointer
                                      (1+ otherweb--current-pointer))
          )
    ))
(defun otherweb--dec-pointer ()
  "Decrease with side effect"
  (if (> otherweb--current-pointer 0)
      (setq otherweb--current-pointer (1- otherweb--current-pointer))
    (setq otherweb--current-pointer 0))
  )
(defun otherweb--next ()
  "Return the data object of next page"
  (interactive)
  (message "LOG:otherweb--nextpage")
  (cond
   ((not otherweb--data)
    (progn
      (add-to-list 'otherweb--data
                                       (otherweb--getdata)
                                       t)
      (otherweb--inc-pointer)
      (elt otherweb--data 0)))
   ((otherweb--point-to-last-item-p)
    (progn
      (let ((id (cdadr (elt otherweb--data
                            (1- (length otherweb--data))))))
         (add-to-list 'otherweb--data
                               (otherweb--getdata id)
                               t)
        (otherweb--inc-pointer)
        (elt otherweb--data
             (1- (length otherweb--data))))))
   (t (progn
        (otherweb--inc-pointer )
        (elt otherweb--data
             otherweb--current-pointer)
        )))
  )
(defun otherweb--prev ()
  "return data object of previous page"
  (interactive)
  (cond
   ((not otherweb--data) (message "Init otherweb first") )
   (t  (elt otherweb--data (otherweb--dec-pointer)))
    ))

(defun otherweb-next ()
  (interactive)
  (otherweb (otherweb--next))
    )
(defun otherweb-prev ()
  (interactive)
  (if (= otherweb--current-pointer 0)
      (message "This is the first page")
      (otherweb (otherweb--prev))))



;;;###autoload
(defun otherweb (&optional page--items)
  "Main function, parse and display items data"
  (interactive)
  (message "LOG: otherweb")
  (with-current-buffer (get-buffer-create "*Test output *")
    (erase-buffer)
    (let* (
           (items (if page--items
                      (cdar page--items)
                    (progn
                      (otherweb--clear-data)
                      (cdar ( otherweb--next)))))
           (total (cdr  (assoc 'total  (cdr  (first otherweb--data)))))
           (page (1+ otherweb--current-pointer))
           )
      (insert (format "#+STARTUP: show2levels
#+TITLE: Other web headline

* Otherweb
" nil))
      (insert (apply #'concat
                     (-map #'otherweb--format-item-org items)))
      (display-buffer (current-buffer))
      (org-mode)
      (goto-line 0)
      (otherweb--setup-header-line  total page otherweb--sort )
      (otherweb-mode)
      (visual-line-mode))))


(provide 'otherweb)
;;; otherweb.el ends here

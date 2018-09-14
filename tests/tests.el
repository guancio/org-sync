(setq org-sync-src-1 (expand-file-name "test1.org"))
(setq org-sync-src-2 (expand-file-name "test2.org"))
(setq org-sync-data (expand-file-name "data.org"))
(setq org-sync-backup-1 (expand-file-name "backup-1.org"))
(setq org-sync-g-query "test-org")

(google-contacts)

(google-contacts-async-api
 "test-org"
 #'(lambda (contacts)
     (mapcar (lambda (contact)
	       (os-delete-g (os-g-getid contact)))
	     contacts)))

(google-contacts-async-api
 "test-org"
 #'(lambda (contacts)
     (cl-assert (= 0 (length contacts)) t "There should not be any test-org contact")))

(delete-file org-sync-data)

(find-file-noselect org-sync-data)

(os-add-g "test-org-5" "1234567" "ciao@gmail.com")
(os-sync)


(use-package "google-contacts"
  :load-path "/home/guancio/Sources/org-sync/google-contacts.el")

(require 'google-oauth)
(require 'url-cache)
(require 'widget)
(require 'xml)
(require 'cl-lib)
(require 'oauth2)


(defvar org-sync-src-1)
(defvar org-sync-src-2)
(defvar org-sync-data)
(defvar org-sync-backup-1)
(defvar org-sync-g-query)


;; Google contact action-methods

(defun os-google-contacts-http-data (buffer code)
  "Return HTTP data from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (let ((headers (buffer-substring (point-min) (point))))
      (unless (string-match-p (format "^HTTP/1.1 %s" code) headers)
        (kill-buffer)
        (error "Unable to fetch data"))
      (if (string-match-p "^Content-Type:.* charset=UTF-8" headers)
          (set-buffer-multibyte t)
        (set-buffer-multibyte nil)))
    (let ((data (buffer-substring (point) (point-max))))
      (kill-buffer)
      data)))

(defun os-google-parse-data (data)
  (with-temp-buffer
    (insert data)
    (xml-parse-region
     (point-min) (point-max))))

(defun os-add-g (item telephone email group)
  (let* ((request-method "POST")
	 (request-data
	  (format "
<atom:entry xmlns:atom=\"http://www.w3.org/2005/Atom\"
    xmlns:gd=\"http://schemas.google.com/g/2005\">
  <atom:category scheme=\"http://schemas.google.com/g/2005#kind\"
    term=\"http://schemas.google.com/contact/2008#contact\"/>
  <gd:name>
    <gd:fullName>%s</gd:fullName>
  </gd:name>
  <gd:email label=\"email\" address=\"%s\"/>
  <gd:phoneNumber label=\"phone\">%s</gd:phoneNumber>
  <gContact:groupMembershipInfo deleted='false'
    href='%s'/>
</atom:entry>
"  item email telephone group))
         (request-extra-headers
          '(("Content-Type" . "application/atom+xml")
	    ("GData-Version" . "3.0")))
	 (token (google-contacts-oauth-token))
	 (buf (oauth2-url-retrieve-synchronously
	       token
	       "https://www.google.com/m8/feeds/contacts/default/full"
	       request-method request-data request-extra-headers))
         (contacts (os-google-parse-data (os-google-contacts-http-data buf "201 Created"))))
    (car (google-contacts-to-list contacts))))


(defun google-groups-to-list (groups &optional token)
  "Convert GROUPS to a list of alists.
A valid TOKEN is required to retrieve photo properties."
  (let (ret)
    (dolist (group groups ret)
      (push `((id . , (xml-node-child-string (nth 0 (xml-get-children group 'id))))
	      (group . , (xml-get-attribute-or-nil (nth 0 (xml-get-children group 'gContact:systemGroup)) 'id))
	      )
            ret))))


(defun os-get-g-groups ()
  (let* ((request-method "GET")
	 (request-data "")
         (request-extra-headers
          '(("Content-Type" . "application/atom+xml")
	    ("GData-Version" . "3.0")))
	 (token (google-contacts-oauth-token))
	 (buf (oauth2-url-retrieve-synchronously
	       token
	       "https://www.google.com/m8/feeds/groups/default/full"
	       request-method request-data request-extra-headers))
	 (text (os-google-contacts-http-data buf "200 OK"))
         (data (os-google-parse-data text))
	 (groups (xml-get-children (assoc 'feed data) 'entry))
	 )
    (google-groups-to-list groups)
    )
  )

(defun os-get-g-main-group ()
  (let ((group (car (seq-filter (lambda (group)
		(equal "Contacts" (cdr (assq 'group group))))
				(os-get-g-groups)))))
    (cdr (assq 'id group))))
    

	

(defun os-delete-g (id)
  (let* ((request-method "DELETE")
         (request-extra-headers
          '(("If-match" . "*")
	    ("GData-Version" . "3.0")))
	 (token (google-contacts-oauth-token))
	 (buf (oauth2-url-retrieve-synchronously
	       token
	       (format "https://www.google.com/m8/feeds/contacts/default/full/%s" id)
	       request-method "" request-extra-headers))
         (res (os-google-contacts-http-data buf "200 OK")))
    res))


;; Google contact utility functions
(defun os-g-get-property (entity contact)
  "Get ENTITY of CONTACT with ATTR-NAME"
  (let ((data (cdr (assq entity contact))))
    (when (and data (not (and (stringp data) (string= data ""))))
      (cond
       ((stringp data)
        data)
       ((listp data)
	(string-join (mapcar (lambda (i) (cdr i)) data) " "))
       ))))


(defun os-g-getid (contact)
  (car (last (split-string
	      (os-g-get-property 'id contact) "/"))))
  

;; Google contact management
(defun os-find-g-in-d (contact)
  (with-current-buffer (find-file-noselect org-sync-data)
    (org-map-entries 'point (format "+GID=\"%s\"" (os-g-getid contact)) 'file)
    )
  )
(defun os-to-add-g-d-p (contact)
  (and (not (eq nil (os-g-get-property 'fullname contact)))
       (eq (os-find-g-in-d contact) nil)))

(defun os-add-g-d (contact id)
  (let* ((item (os-g-get-property 'fullname contact))
	 (telephone (os-g-get-property 'phones contact))
	 (email (os-g-get-property 'emails contact))
	 (g-id (os-g-getid contact))
	 )
      (with-current-buffer (find-file-noselect org-sync-data)
	(progn
	  (goto-char (point-max))
	  (insert (format "\n* %s" item))
	  (org-entry-put (point) "TELEPHONE" telephone)
	  (org-entry-put (point) "EMAIL" email)
	  (org-entry-put (point) "GID" g-id)
	  (org-entry-put (point) "ID" id)
	  ))
      ))
(defun os-add-g-1 (contact)
  (let* ((item (os-g-get-property 'fullname contact))
	 (telephone (os-g-get-property 'phones contact))
	 (email (os-g-get-property 'emails contact))
	 (g-id (os-g-getid contact))
	 )
      (with-current-buffer (find-file-noselect org-sync-src-1)
	(progn
	  (goto-char (point-max))
	  (insert (format "\n* %s" item))
	  (org-entry-put (point) "TELEPHONE" telephone)
	  (org-entry-put (point) "EMAIL" email)
	  (org-entry-put (point) "GID" g-id)
	  (org-id-get-create)
	  ))
      ))

;; org-management
(defun os-find-1-in-d (ID)
  (with-current-buffer (find-file-noselect org-sync-data)
    (org-map-entries 'point (format "+ID=\"%s\"" ID) 'file)
    )
  )

(defun os-to-add-1-d-p ()
  (with-current-buffer (find-file-noselect org-sync-src-1)
    (let* ((ID (org-entry-get (point) "ID")))
      (or (eq ID nil)
	  (eq (os-find-1-in-d ID) nil)
	  )
    ))
  )

(defun os-add-1-d (id-g)
  (with-current-buffer (find-file-noselect org-sync-src-1)
    (let* ((item (org-entry-get (point) "ITEM"))
	   (telephone (org-entry-get (point) "TELEPHONE"))
	   (email (org-entry-get (point) "EMAIL"))
	   (id (org-entry-get (point) "ID"))
	   (id1 (org-id-get-create))
	   )
      (with-current-buffer (find-file-noselect org-sync-data)
	(progn
	  (goto-char (point-max))
	  (insert (format "\n* %s" item))
	  (org-entry-put (point) "TELEPHONE" telephone)
	  (org-entry-put (point) "EMAIL" email)
	  (org-entry-put (point) "ID" id1)
	  (org-entry-put (point) "GID" id-g)
	  ))
      ))
  )

(defun os-add-1-g (group)
  (with-current-buffer (find-file-noselect org-sync-src-1)
    (let* ((item (org-entry-get (point) "ITEM"))
	   (telephone (org-entry-get (point) "TELEPHONE"))
	   (email (org-entry-get (point) "EMAIL"))
	   (id (org-entry-get (point) "ID"))
	   (id1 (org-id-get-create))
	   (contact (os-add-g item telephone email group)))
      contact)))

(defun os-sync (main-group-id)
  (interactive)
  (with-current-buffer (find-file-noselect org-sync-src-1)
    (org-map-entries
     (lambda ()
       (if (os-to-add-1-d-p)
	   (let ((id-g (os-g-getid (os-add-1-g main-group-id))))
	     (os-add-1-d id-g))))))
  (google-contacts-async-api
   org-sync-g-query
   #'(lambda (contacts)
       (mapcar
	(lambda (contact)
	  (if (os-to-add-g-d-p contact)
	      (let ((id (os-add-g-1 contact)))
		(os-add-g-d contact id)))
	  )
	contacts)))
  )

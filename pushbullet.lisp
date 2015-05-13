(defpackage :pushbullet
  (:use :cl :alexandria :iterate)
  (:import-from :drakma)
  (:import-from :st-json)
  (:export #:*pushbullet-api-key*
           #:list-devices
           #:find-device
           #:list-contacts
           #:find-contact
           #:send-notification
           #:send-notification-to-device))

(in-package :pushbullet)

(defvar *pushbullet-api-key*)
(defvar *contact-cache* (make-hash-table :test #'equal))
(defvar *device-cache* (make-hash-table :test #'equal))

(defun dasherize (s)
  (make-keyword (substitute #\- #\_ (string-upcase s))))

(defun ensure-string-not-octets (thing)
  (if (stringp thing)
      thing
      (babel:octets-to-string thing :encoding :utf-8)))

(defun simplify-json (json)
  (typecase json
    (st-json:jso
     (let ((result '()))
       (st-json:mapjso
        #'(lambda (k v)
            (push (dasherize k) result)
            (push (simplify-json v) result))
        json)
       (nreverse result)))
    (proper-list
     (mapcar #'simplify-json json))
    (t json)))

(defun pushbullet-request (method place &optional parameters)
  (simplify-json
   (st-json:read-json-from-string
    (ensure-string-not-octets
     (drakma:http-request (concatenate 'string "https://api.pushbullet.com/v2/" place)
                          :method method
                          :basic-authorization (list *pushbullet-api-key* "")
                          :parameters parameters
                          :external-format-in :utf-8
                          :external-format-out :utf-8)))))

(defun list-devices ()
  (getf (pushbullet-request :get "devices") :devices))

(defun find-device (name)
  (values
   (ensure-gethash
    name *device-cache*
    (iter (for device in (list-devices))
          (when (string-equal name (getf device :nickname))
            (return (getf device :iden)))
          (finally (error "can't find pushbullet device: ~s" name))))))

(defun list-contacts ()
  (getf (pushbullet-request :get "contacts") :contacts))

(defun find-contact (name)
  (values
   (ensure-gethash
    name *contact-cache*
    (iter (for contact in (list-contacts))
          (when (or (string-equal name (getf contact :name))
                    (string-equal name (getf contact :email-normalized)))
            (return (getf contact :email-normalized)))
          (finally (error "can't find pushbullet contact: ~s" name))))))

(defun send-notification (to title text)
  (pushbullet-request
   :post "pushes"
   (list (cons "type" "note")
         (cons "email" (find-contact to))
         (cons "title" title)
         (cons "body" text))))

(defun send-notification-to-device (to title text)
  (pushbullet-request
   :post "pushes"
   (list (cons "type" "note")
         (cons "device_iden" (find-device to))
         (cons "title" title)
         (cons "body" text))))

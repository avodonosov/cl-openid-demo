(defpackage :openid-demo
  (:use :cl)
  (:export :start))

(in-package :openid-demo)

(defun src-rel-path (subpath)
  (asdf:system-relative-pathname :openid-demo subpath))

(defclass demo-acceptor (hunchentoot:easy-acceptor)
  ((relying-party :type cl-openid:relying-party
                  :initarg :relying-party
                  :accessor relying-party
                  :initform (error ":relying-party is required"))))

(defun cur-user ()
  (and hunchentoot:*session*
       (hunchentoot:session-value 'cur-user)))

(hunchentoot:define-easy-handler (home :uri "/")
    ()
  (format nil
          "<!DOCTYPE HTML>
<html>
  <head><title>CL OpenID Demo</title></head>
  <body>
    <div><img src=\"http://www.lisperati.com/lisplogo_alien_128.png\"
              style=\"vertical-align:middle;\"/>
    Hello Friend!
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"/login\">(re)login</a>
    </div>
    <p>You are authenticated as:<br/> <code>~S</code></p>
  </body>
</html>"
          (cur-user)))

(push (hunchentoot:create-folder-dispatcher-and-handler "/jquery-openid/"
                                                        (src-rel-path  "jquery-openid/"))
      hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler (send-notification :uri "/login")
    (openid_identifier)
  (if openid_identifier ;; form submited, initiate authentication      
      ;; We can request not only user identity, but also additional
      ;; attributes as email, first/last names, country, language, etc.
      ;; This may be done via OpenID extensions: 
      ;; OpenID Simple Registration Extension or OpenID Attribute Exchange Extension.
      ;; We use both extensions, for sure, as different providers may support
      ;; one extension but not another.
      (let ((attr-exchange '(:openid.ns.ax "http://openid.net/srv/ax/1.0"
                             :openid.ax.mode "fetch_request"
                             :openid.ax.type.email "http://axschema.org/contact/email"
                             :openid.ax.type.language "http://axschema.org/pref/language"
                             :openid.ax.type.country "http://axschema.org/contact/country/home"
                             :openid.ax.type.firstname "http://axschema.org/namePerson/first"
                             :openid.ax.type.lastname  "http://axschema.org/namePerson/last"
                             ;; choose the attributes you want to request
                             ;; in the followin comma separated list:
                             :openid.ax.required "email,language,country,firstname,lastname"
                             ;; attributes that we want but do not require may be requested
                             ;; as if_available, although for example Google doesn't support
                             ;; if_available today (see https://developers.google.com/accounts/docs/OpenID,
                             ;; section "Attribute exchange extension" for the list what google support)
                             ;;
                             ;;:openid.ax.if_available ""
                             ))
            (simple-reg '(:openid.ns.sreg "http://openid.net/extensions/sreg/1.1"
                          :openid.sreg.optional "nickname,email,fullname,dob,gender,postcode,country,language,timezone")))
        (hunchentoot:REDIRECT            
         (cl-openid:initiate-authentication (relying-party hunchentoot:*acceptor*) 
                                            openid_identifier
                                            :extra-parameters (append attr-exchange
                                                                      simple-reg))))
      ;; else - render the form
      (format nil
              "<!DOCTYPE HTML>
<html>
<head>
  <title>OpenID Login</title>
  <script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-1.6.4.js\"></script>
  <script type=\"text/javascript\" src=\"/jquery-openid/jquery.openid.js\"></script>
  <link href=\"/jquery-openid/openid.css\" rel=\"stylesheet\" type=\"text/css\">
</head>
<body>
  ~A
</body>
</html>"
              (alexandria:read-file-into-string (src-rel-path "jquery-openid/login-form.html")))))

(defun make-account (open-id-identity response-message)
  "Unify attributes representation of the two extensions:
OpenID Simple Registration Extension or OpenID Attribute Exchange Extension."
  (labels ((val (key)
             (cdr (assoc key response-message :test #'string=)))
           (or-val (key1 key2)
             (or (val key1) (val key2))))
    (list :claimed-id open-id-identity
          :email (or-val "openid.sreg.email" "openid.ext1.value.email")
          :nickname (val "openid.sreg.nickname")
          :fullname (val "openid.sreg.fullname")
          :firstname (val "openid.ext1.value.firstname")
          :lastname (val "openid.ext1.value.lastname")

          :birthday (val "openid.sreg.dob")
          :country (or-val "openid.sreg.country" "openid.ext1.value.country")
          :language (or-val "openid.sreg.language" "openid.ext1.value.language")
          :timezone (val "openid.sreg.timezone")
          :postcode (val "openid.sreg.postcode")))

  ;; Note,
  ;; 
  ;; Simple Registration Extension defines only 9 attributes,
  ;; we use all of them - those starting with openid.sreg.
  ;; 
  ;; OpenID Attribute Exchange is an extensible framework, many attributes
  ;; are defined here: http://openid.net/specs/openid-attribute-properties-list-1_0-01.html
  ;; In our example we only use the attributes supported by Google  
  )

(hunchentoot:define-easy-handler (openid-rp :uri "/openid-rp")
    ()
  (let* (;; hunchentoot GET paremeters have the same 
         ;; representation as open-id message: an alist
         (message (hunchentoot:get-parameters hunchentoot:*request*)) 
         (absolute-reply-uri (puri:merge-uris (hunchentoot:request-uri hunchentoot:*request*) 
                                              (cl-openid:root-uri *relying-party*)))
         user-id-url
         authproc)
    (format t "response message: ~% ~{~s~^~% ~}~%" message)
    (finish-output)
    (handler-case 
        (setf (values user-id-url authproc) 
              (cl-openid:handle-indirect-response *relying-party* 
                                                  message
                                                  absolute-reply-uri))
      (cl-openid:openid-assertion-error (e)
        (RETURN-FROM openid-rp (format nil "Error: ~A ~A"
                                       (cl-openid:code e)
                                       e)))
      (t (e) (RETURN-FROM openid-rp (format nil "Error: ~A" e))))
    (if user-id-url
        (progn         
          (format t "(type-of user-id-url): ~A~%" (type-of user-id-url))
          ;; todo for cl-openid: return user ID as a string instead puri:uri
          (setf user-id-url (princ-to-string user-id-url))
          
          (setf (hunchentoot:session-value 'cur-user)
                (make-account user-id-url message))
          (hunchentoot:REDIRECT "/"))
        ;; else:
        "Access denied")))

(defun make-relying-party (public-host public-port)
  (let ((host-port (format nil "~A:~A" public-host public-port)))
    (make-instance 'cl-openid:relying-party
                   :root-uri (puri:uri (format nil 
                                               "http://~A/openid-rp"
                                               host-port))
                   :realm (puri:uri (format nil "http://~A"
                                            host-port)))
    ;; todo for cl-openid: allow the URIs to be just strings
    ))


(defun start (&key port public-host (public-port port))
  "
  PORT is the TCP port we open socket at.

  PUBLIC-HOST is the host name through wich user's browser access our application;
              you can use \"localhost\" during development.
            
  PUPLIC-PORT is the port on wich user's browser access our application
              (may be different from PORT for exmaple at Heroku)."

  (hunchentoot:start (make-instance 'demo-acceptor
                                    :port port
                                    :relying-party (make-relying-party public-host public-port))))
;;; init.el --- My init.el file

;;; Commentary:
;;; None

;;; code:

(define-minor-mode openfortivpn-global-mode
  "Openfortivpn mode."
  :global t
  :lighter (:eval (openfortivpn-lightuer)))

(defvar openfortivpn-path "openfortivpn"
  "Path of openfortivpn.")

(defvar openfortivpn-args nil
  "List of openfortivpn arguments.")

(defvar openfortivpn-trusted-certs nil
  "List of trusted certificates.")

(defvar openfortivpn-server-alist nil
  "Alist of FortiVPN servers.")

(defvar openfortivpn-current-server-id nil
  "Current server ID.")

(defun openfortivpn-lightuer ()
  (concat " FortiVPN[" openfortivpn-current-server-id "]"))

(defun openfortivpn-get-server (srv-id)
  "Get server(SRV-ID) from list."
  (cdr (assoc srv-id openfortivpn-server-alist)))

(defun openfortivpn-connect-p ()
  (process-live-p (get-process "openfortivpn")))

(defun openfortivpn-send-sudo-password (proc)
  (process-send-string proc sudo)
  (process-send-string proc "\r")
  (process-send-eof proc))

(defun openfortivpn-start-process (srv-id username password sudo)
  (interactive (list (read-string "Server ID: ") (read-string "Username: ") (read-passwd "Password: ") (read-passwd "Sudo: ")))
  (let ((proc
         (apply 'start-process
                "openfortivpn"
                (get-buffer-create "*openfortivpn*")
                (append
                 (list "sudo"
                       openfortivpn-path
                       (openfortivpn-get-server srv-id)
                       "--username" username
                       "--password" password)
                 (apply #'append (mapcar #'(lambda (c) (append '("--trusted-cert") (list c))) openfortivpn-trusted-certs))
                 openfortivpn-args))))
    (openfortivpn-send-sudo-password proc)
    (setq openfortivpn-current-server-id srv-id)
    (openfortivpn-global-mode t)))
  
(defun openfortivpn-connect ()
  (interactive)
  (if (openfortivpn-connect-p)
      (message "FortiVPN is already connected.")
      (call-interactively 'openfortivpn-start-process)))

(defun openfortivpn-kill-process (sudo)
  (interactive (list (read-passwd "Sudo: ")))
  (let* ((pid (process-id (get-process "openfortivpn")))
         (proc (start-process "kill-openfortivpn" nil "sudo" "kill" (number-to-string pid))))
    (openfortivpn-send-sudo-password proc)
    (setq openfortivpn-current-server-id nil)
    (openfortivpn-global-mode -1)))

(defun openfortivpn-disconnect ()
  (interactive)
  (if (openfortivpn-connect-p)
      (call-interactively 'openfortivpn-kill-process)
    (message "FortiVPN is not connected")))

(provide 'openfortivpn)

;;; openfortivpn.el ends here

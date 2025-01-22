(define-module (paulj systems base users)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages shells)
  #:use-module (gnu system accounts)
  #:use-module (guix gexp)
  #:export (paul))

(define paul
  (user-account
   (name "paul")
   (comment "Paul Jewell")
   (group "users")
   (home-directory "/home/paul")
   (supplementary-groups '("wheel" "netdev" "lp" "audio" "video" "plugdev"))))

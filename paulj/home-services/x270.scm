;; x280.scm - configuration for Lenovo x270 system
(define-module (paulj home-services x270)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:export (home-x270-service-type))

(define (home-x270-profile-service config)
  (list brightnessctl))

(define home-x270-service-type
  (service-type (name 'home-x270)
                (description "Configuration for Lenovo X270.")
                (extensions
                 (list (service-extension
                        home-profile-service type home-x270-profile-service)))
                (default-value #f)))


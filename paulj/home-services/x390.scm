;; x390.scm - configuration for Lenovo x390 system
(define-module (paulj home-services x390)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:export (home-x390-service-type))

(use-package-modules linux)

(define (home-x390-profile-service config)
  (list brightnessctl))

(define home-x390-service-type
  (service-type (name 'home-x390)
                (description "Configuration for Lenovo X390.")
                (extensions
                 (list (service-extension
                        home-profile-service-type home-x390-profile-service)))
                (default-value #f)))


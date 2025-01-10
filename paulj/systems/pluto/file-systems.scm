(define-module (paulj systems pluto file-systems)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system uuid)
  #:export (%pluto-file-systems))
  
(define root
  (file-system
   (mount-point "/")
   (device (uuid
            "a5e463ab-8489-42e2-bf20-c3fad9423ea7"
            'xfs))
   (type "xfs")))

(define boot  
  (file-system
   (mount-point "/boot/efi")
   (device (uuid "7557-8745"
                 'fat32))
   (type "vfat")))

(define %pluto-file-systems
  (cons* root
         boot
         %base-file-systems))

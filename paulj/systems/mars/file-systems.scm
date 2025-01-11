(define-module (paulj systems mars file-systems)
  #:use-module (gnu system file-systems)
  #:export (%btrfs-file-systems))

(define guix-pool-part
  (file-system-label "guix-pool"))
  
(define guix-boot-part
  (file-system-label "guix-boot"))

(define root
  (file-system
   (device guix-pool-part)
   (mount-point "/")
   (type "btrfs")
   (options "subvol=@root,ssd")))

(define gnu
  (file-system
   (device guix-pool-part)
   (mount-point "/gnu")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=@gnu,compress=zstd,ssd")))

(define var-log
  (file-system
   (device guix-pool-part)
   (mount-point "/var/log")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=@log,compress=zstd,ssd")))

(define home
  (file-system
   (device guix-pool-part)
   (mount-point "/home")
   (type "btrfs")
   (options "subvol=@home")))

(define boot
  (file-system
   (device guix-boot-part)
   (mount-point "/boot/efi")
   (check? #f)
   (type "vfat")))

(define %btrfs-file-systems
  (cons* root
         gnu
         var-log
         home
         boot
         (delete %debug-file-system
                 %base-file-systems)))


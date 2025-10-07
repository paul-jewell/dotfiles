(define-module (paulj systems venus file-systems)
  #:use-module (gnu system file-systems)
  #:export (%btrfs-file-systems))

(define pool-partition
  (file-system-label "pool"))
  
(define boot-partition
  (file-system-label "guix-boot"))

(define root
  (file-system
   (device pool-partition)
   (mount-point "/")
   (type "btrfs")
   (options "subvol=@root,ssd")))

(define gnu
  (file-system
   (device pool-partition)
   (mount-point "/gnu")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=@gnu,compress=zstd,ssd")))

(define var-log
  (file-system
   (device pool-partition)
   (mount-point "/var/log")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=@log,compress=zstd,ssd")))

(define home
  (file-system
   (device pool-partition)
   (mount-point "/home")
   (type "btrfs")
   (options "subvol=@home")))

(define boot
  (file-system
   (device boot-partition)
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


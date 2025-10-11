(define-module (paulj systems venus file-systems)
  #:use-module (gnu system file-systems)
  #:export (%btrfs-file-systems))

(define boot-partition
  (file-system-label "guix-boot"))

# TODO: Add source name
(define venus-mapped-devices
  (list
    (mapped-device 
      (source ())
      (target "enc")
      (type luks-device-mapping))))

(define root
  (file-system
   (device "/dev/mapper/enc")
   (mount-point "/")
   (type "btrfs")
   (needed-for-boot? #t)
   (options "subvol=@root,ssd")))

(define gnu
  (file-system
   (device "/dev/mapper/enc")
   (mount-point "/gnu")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=@gnu,compress=zstd,ssd")))

(define var-log
  (file-system
   (device "/dev/mapper/enc")
   (mount-point "/var/log")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=@log,compress=zstd,ssd")))

(define home
  (file-system
   (device "/dev/mapper/enc")
   (mount-point "/home")
   (type "btrfs")
   (options "subvol=@home")))

(define swap
  (file-system
    (device "/dev/mapper/enc")
    (mount-point "/swap")
    (type "btrfs")
    (options "subvol=@swap")))

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
         swap
         (delete %debug-file-system
                 %base-file-systems)))


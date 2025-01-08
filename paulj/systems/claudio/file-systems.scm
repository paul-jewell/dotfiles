(define-module (paulj systems claudio file-systems)
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

(define gnu-store
  (file-system
   (device guix-pool-part)
   (mount-point "/gnu/store")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=gnu-store,compress=zstd,ssd")))

(define var-log
  (file-system
   (device guix-pool-part)
   (mount-point "/var/log")
   (type "btrfs")
   (flags '(no-atime))
   (needed-for-boot? #t)
   (options "subvol=@var/log,compress=zstd,ssd")))

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
   (type "vfat")))

(define tmp
  (file-system
   (device "none")
   (mount-point "/tmp")
   (type "tmpfs")
   (check? #f)))

(define run
  (file-system
   (device "none")
   (mount-point "/run")
   (type "tmpfs")
   (options "mode=0755")
   (needed-for-boot? #t)
   (check? #f)))

(define var-run
  (file-system
   (device "none")
   (mount-point "/var/run")
   (type "tmpfs")
   (options "mode=0755")
   (needed-for-boot? #t)
   (check? #f)))

(define %btrfs-file-systems
  (cons* root
	 gnu-store
	 var-log
         home
         boot
         tmp
         run
         var-run
         (delete %debug-file-system
                 %base-file-systems)))


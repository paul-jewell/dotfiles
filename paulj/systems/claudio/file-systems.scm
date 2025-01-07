(define-module (paulj systems claudio file-systems)
  #:use-module (gnu system file-systems)
  #:export (%btrfs-ephemeral-file-systems))

(define guix-part
  (file-system-label "guix"))

(define efi-part
  (file-system-label "guix-boot"))

(define root
  (file-system
   (device "none")
   (mount-point "/")
   (type "tmpfs")
   (check? #f)
   (needed-for-boot? #t)
   (options "mode=0755")))

(define home
  (file-system
   (device guix-part)
   (type "btrfs")
   (mount-point "/home")
   (flags '(no-atime))
   (options "subvol=@home,discard=async,ssd")))

(define root-user
  (file-system
   (device guix-part)
   (type "btrfs")
   (mount-point "/root")
   (flags '(no-atime))
   (options "subvol=@root,discard=async,ssd")))

(define boot
  (file-system
   (device guix-part)
   (mount-point "/boot")
   (type "btrfs")
   (check? #f)
   (needed-for-boot? #t)
   (flags '(no-atime))
   (options "subvol=@boot,discard=async,ssd")))

(define boot-efi
  (file-system
   (device efi-part)
   (type "vfat")
   (mount-point "/boot/efi")))

(define tmp
  (file-system
   (device "none")
   (mount-point "/tmp")
   (type "tmpfs")
   (check? #f)
   (needed-for-boot? #f)))

(define run
  (file-system
   (device "none")
   (mount-point "/run")
   (type "tmpfs")
   (needed-for-boot? #t)
   (check? #f)
   (options "mode=0755")))

(define var-run
  (file-system
   (device "none")
   (mount-point "/var/run")
   (type "tmpfs")
   (needed-for-boot? #t)
   (check? #f)
   (options "mode=0755")))

(define var-log
  (file-system
   (device guix-part)
   (type "btrfs2")
   (mount-point "/var/log")
   (check? #f)
   (needed-for-boot? #t)
   (flags '(no-atime))
   (options "compress=zstd,subvol=@var/log,ssd")))

(define var-lib
  (file-system
   (device guix-part)
   (type "btrfs2")
   (mount-point "/var/lib")
   (needed-for-boot? #t)
   (flags '(no-atime))
   (options "compress=zstd,subvol=@var/lib,ssd")))

(define var-guix
  (file-system
   (device guix-part)
   (type "btrfs2")
   (mount-point "/var/guix")
   (needed-for-boot? #t)
   (flags '(no-atime))
   (options "compress=zstd,subvol=@var/guix,ssd")))

(define gnu-store
  (file-system
   (device guix-part)
   (type "btrfs2")
   (mount-point "/gnu/store")
   (needed-for-boot? #t)
   (flags '(no-atime))
   (options "compress=zstd,subvol=@gnu/store,ssd")))

(define gnu-persist
  (file-system
   (device guix-part)
   (type "btrfs")
   (mount-point "/gnu/persist")
   (needed-for-boot? #t)
   (flags '(no-atime))
   (options"subvol=@gnu/persist,ssd")))

(define gnu-persist-ssh
  (file-system
   (device "/gnu/persist/etc/ssh")
   (type "none")
   (mount-point "/etc/ssh")
   (flags '(no-atime bind-mount))))

(define gnu-persist-guix
  (file-system
   (device "/gnu/persist/etc/guix")
   (type "none")
   (mount-point "/etc/guix")
   (flags '(no-atime bind-mount))))

(define gnu-persist-wireguard
  (file-system
   (device "/gnu/persist/etc/wireguard")
   (type "none")
   (mount-point "/etc/wireguard")
   (flags '(no-atime bind-mount))))


(define %btrfs-ephemeral-file-systems
  (cons* root
         home
	 root-user
         boot
	 boot-efi
         tmp
         run
         var-run
	 var-log
	 var-lib
	 var-guix
	 gnu-store
	 gnu-persist
	 gnu-persist-ssh
	 gnu-persist-guix
	 gnu-persist-wireguard
         (delete %debug-file-system
                 %base-file-systems)))


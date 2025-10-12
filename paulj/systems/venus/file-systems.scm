(define-module (paulj systems venus file-systems)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (ice-9 match)
  #:export (%btrfs-file-systems))

(define boot-partition
  (file-system-label "boot-part"))

(define venus-mapped-devices
  (list
    (mapped-device 
      (source "/dev/nvme0n1p2")
      (target "enc")
      (type luks-device-mapping))))

(define boot
  (file-system
   (device boot-partition)
   (mount-point "/boot/efi")
   (check? #f)
   (type "vfat")))

(define btrfs-subvolumes
  (map (match-lambda
         ((subvol . mount-point)
          (file-system
            (type "btrfs")
            (device "/dev/mapper/enc")
            (mount-point mount-point)
            (options (format #f "subvol=~a" subvol))
            (dependencies venus-mapped-devices))))
       '((@ . "/")
         (@boot . "/boot")
         (@home . "/home")
         (@gnu . "/gnu")
         (@log . "/var/log")
         (@swap . "/swap")
         (@data . "/data"))))

(define %btrfs-file-systems
  (append 
    btrfs-subvolumes
    boot))

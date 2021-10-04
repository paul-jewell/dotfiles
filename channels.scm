;; NOTE: This file is generated from ~/dotfiles/System.org.  Please see commentary there.

(list (channel
       (name 'pj-suckless)
       (url "https://github.com/paul-jewell/pj-suckless.git"))
      (channel
       (name 'nonguix)
       ;;(commit "c34fa8bfacdce5fa45b2a684c2b27309c09a9056")
       (url "https://gitlab.com/nonguix/nonguix")
       ;; enable signature verification
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix)
       ;;(commit "190187326ad7516dd6728eed7bb6ef2d4f92897a")
       (url "https://git.savannah.gnu.org/git/guix.git")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

let
  paul = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO1Ia1H2cMVFPkJYl7EedhCQen2u8CdbUJYHh39STkRR paul@isolde"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH4HJWyXzUw6EW66y81iRjwXFAqR9wwxRvuZw/W7NBy2 paul@isolde"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINX9KVaqmzbp8eb6WgIy5gXlNCK3XKQtDmBHEghZgEDD paul@isolde"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOau2hlmZjd2UtWd1Yxz6a/S7iyidcmWq8q1QH4TNhD5 paul@isolde"
  ];

  isolde = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKYlGOi/nu3U4pORGwJuVjanMZMPX3qHcPYH4LIvj0UP root@nixos"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEwSXiI0/VUx0B9auVaCB6tDU8AP7QLbgOFQaH8khRnA"
  ];
in {
  "isolde/wg.age".publicKeys = isolde ++ paul;

  "common/beets-secrets.yaml".publicKeys = isolde ++ paul;
  "common/listenbrainz-token".publicKeys = isolde ++ paul;
}

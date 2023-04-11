let
  paul = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnzt/bubLD3jz9wHlXJ6K0ocRBPHyfd/z5kxzt0w6m8 paul@isolde"
  ];

  isolde = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKYlGOi/nu3U4pORGwJuVjanMZMPX3qHcPYH4LIvj0UP"
  ];
in {
  "isolde/wg.age".publicKeys = isolde ++ paul;

  "common/beets-secrets.yaml".publicKeys = isolde ++ paul;

  #  "common/listenbrainz-token".publicKeys = andromeda ++ dyson ++ scrumplex;
}

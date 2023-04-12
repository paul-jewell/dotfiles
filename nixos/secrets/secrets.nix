let
  paul = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnzt/bubLD3jz9wHlXJ6K0ocRBPHyfd/z5kxzt0w6m8 paul@isolde"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHGM/m1oY/ZL3XoD1E6I/0uDIK1A8kWoO5B1oqjaGKqB paul@gandalf"
  ];

  isolde = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKYlGOi/nu3U4pORGwJuVjanMZMPX3qHcPYH4LIvj0UP"
  ];
  gandalf = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGb9rcixQQXJbNW2uggbheZr9EZ0BMw88ahgbP4HFnCw"
  ];
in {
  "isolde/wg.age".publicKeys = isolde ++ paul;

  "gandalf/wg.age".publicKeys = gandalf ++ paul;

  "common/beets-secrets.yaml".publicKeys = isolde ++ paul ++ gandalf;

  #  "common/listenbrainz-token".publicKeys = andromeda ++ dyson ++ scrumplex;
}

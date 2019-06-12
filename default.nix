with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "NMEA";
};

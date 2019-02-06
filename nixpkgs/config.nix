{
  allowUnfree = true;
  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = true;
  };
  packageOverrides = pkgs: rec {
    enpass = import ./enpass/default.nix;
  };
}

{ pkgs, ... }:

{
  users.extraUsers.binh = {
    createHome = true;
    home = "/home/binh";
    group = "users";
    extraGroups = [ "wheel" "disk" "networkmanager" "video" "audio" "input" "docker" "vboxusers" "wireshark" ];
    isNormalUser = true;
    uid = 1000;
    useDefaultShell = true;
    shell = pkgs.zsh;
  };

  security.sudo.enable = true;
}

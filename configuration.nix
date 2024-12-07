# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

# NixOS-WSL specific options are documented on the NixOS-WSL repository:
# https://github.com/nix-community/NixOS-WSL


# from https://wiki.nixos.org/wiki/Python#Running_compiled_libraries
{ 
  config, 
  lib, 
  pkgs, 
  ... 
}:
let
  python = pkgs.python311;

  pythonldlibpath = lib.makeLibraryPath (with pkgs; [
    zlib
    zstd
    stdenv.cc.cc
    curl
    openssl
    attr
    libssh
    bzip2
    libxml2
    acl
    libsodium
    util-linux
    xz
    systemd
    imagemagick
  ]);
  patchedpython = (python.overrideAttrs (
    previousAttrs: {
      # Add the nix-ld libraries to the LD_LIBRARY_PATH.
      # creating a new library path from all desired libraries
      postInstall = previousAttrs.postInstall + ''
        mv  "$out/bin/python3.11" "$out/bin/unpatched_python3.11"
        cat << EOF >> "$out/bin/python3.11"
        #!/run/current-system/sw/bin/bash
        export LD_LIBRARY_PATH="${pythonldlibpath}"
        exec "$out/bin/unpatched_python3.11" "\$@"
        EOF
        chmod +x "$out/bin/python3.11"
      '';
    }
  ));
  patchedpoetry =  ((pkgs.poetry.override { python3 = patchedpython; }).overrideAttrs (
    previousAttrs: {
      # note that if you dont keep the blank line bellow, it crashes :(
      postInstall = previousAttrs.postInstall + ''

        mv "$out/bin/poetry" "$out/bin/unpatched_poetry"
        cat << EOF >> "$out/bin/poetry"
        #!/run/current-system/sw/bin/bash
        export LD_LIBRARY_PATH="${pythonldlibpath}"
        exec "$out/bin/unpatched_poetry" "\$@"
        EOF
        chmod +x "$out/bin/poetry"
      '';
    }
  ));
in
{
  # Some other config...
  
  environment.systemPackages = with pkgs; [
    patchedpython
    patchedpoetry
    gcc
    gfortran
    gnuplot
    ruby
  ];

  imports = [
    # include NixOS-WSL modules
    <nixos-wsl/modules>
  ];


  wsl.enable = true;
  wsl.defaultUser = "nixos";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}


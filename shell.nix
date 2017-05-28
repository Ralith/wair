with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "wair";
  buildInputs = (with pkgs; [ rustChannels.stable.rust libevdev systemd libxkbcommon python3 pkgconfig xlibs.libX11 xlibs.libXi xlibs.libXext ]);
}

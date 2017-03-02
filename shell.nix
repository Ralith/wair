with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "wair";
  buildInputs = (with pkgs; [ (rustNightly.rust { }) libevdev systemd libxkbcommon python3 pkgconfig xlibs.libX11 xlibs.libXi xlibs.libXext ]);
}

with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "wair";
  buildInputs = (with pkgs; [ (rustNightly.rust { date = "2016-11-17"; }) libevdev systemd libxkbcommon python3 pkgconfig xlibs.libX11 xlibs.libXi xlibs.libXext ]);
}

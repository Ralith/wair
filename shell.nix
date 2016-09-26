with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "wair";
  buildInputs = (with pkgs; [ cargo libevdev systemd libxkbcommon python3 pkgconfig ]);
  propagatedBuildInputs = (with pkgs; [ xlibs.libX11 xlibs.libXi xlibs.libXext ]);
  preConfigure = ''
    export NIX_LDFLAGS="-rpath ${pkgs.xlibs.libX11}/lib -rpath ${pkgs.xlibs.libXi}/lib $NIX_LDFLAGS"
  '';
}

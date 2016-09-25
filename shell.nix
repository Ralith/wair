with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "wair";
  buildInputs = (with pkgs; [ cargo libevdev systemd libxkbcommon ]);
  propagatedBuildInputs = (with pkgs; [ xlibs.libX11 xlibs.libXi xlibs.libXext ]);
  preConfigure = ''
    export NIX_LDFLAGS="-rpath ${pkgs.xlibs.libX11}/lib $NIX_LDFLAGS"
  '';
}

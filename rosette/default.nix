{ nixpkgs ? import <nixpkgs> {} }:

let

  pkgs = nixpkgs.pkgs.pkgsi686Linux;

in pkgs.stdenv.mkDerivation {

  name = "rosette";

  src = ./.;

  NIX_CFLAGS_COMPILE = "-O0";

  nativeBuildInputs = with pkgs;
    [ cmake
      doxygen
      pkgconfig
    ];

  buildInputs = with pkgs;
    [ # deps go here
      protobuf
    ];

  doCheck = true;

  preCheck = ''
    ulimit -s unlimited
  '';

  checkTarget = "test";

  hardeningDisable = [ "all" ];
}

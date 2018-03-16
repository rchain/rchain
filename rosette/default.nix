{ nixpkgs ? import <nixpkgs> {} }:

let

  pkgs = nixpkgs.pkgs.pkgsi686Linux;

in pkgs.stdenv.mkDerivation {

  name = "rosette";

  src = ./.;

  nativeBuildInputs = with pkgs;
    [ cmake
      doxygen
      pkgconfig
    ];

  buildInputs =
    [ # deps go here
    ];

  doCheck = true;

  checkTarget = "test";
}

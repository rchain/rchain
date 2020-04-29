let

  pkgs = import <nixpkgs> {};

  jdk = pkgs.openjdk8;

  sbt = pkgs.sbt.override { jre = jdk.jre; };

in rec {

  rchainEnv = pkgs.buildFHSUserEnv {
    name = "rchain";
    targetPkgs = ps: rchainPackages;
  };

  rchainPackages = with pkgs; [ haskellPackages.BNFC git jflex sbt jdk ];
}

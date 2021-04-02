# RChain project setup/build using *nix*

This document describes developer setup and build commands for RChain project using _nix_ package manager to install additional tools.

**Note:** _default.nix_ file in this directory is not part of this document.

## Java 11

```sh
sudo update-alternatives --config java

# If necessary install Java 11 version
sudo apt install default-jdk
```

## Nix

Install Nix https://nixos.org/download.html

```sh
curl -L https://nixos.org/nix/install | sh
```

Test `nix` installation

```sh
# Install example Hello World program
nix-env -i hello

# Execute hello program
hello
# Hello, world!

# Unistall Hello World program
nix-env -e hello
```

## sbt

Install Scala build tool `sbt`

```sh
sudo apt install sbt
```

## BNFC

Install `jflex` and `bnfc` with *nix*

```sh
# Install BNFC and jflex with nix
# - jflex v1.7.0 with ghc 8.6.5
nix-env -i jflex -iA haskellPackages.BNFC --file https://github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz

# Uninstall
nix-env -e jflex BNFC

# Install in case of error (Ubuntu)
sudo apt-get install libgmp3-dev
```

## RNode build

```sh
# Regenerate parser from bnfc (./rholang/src/main/java)
# - Java code not part of _target_ folder
sbt bnfc:generate

# Compile
sbt compile

# Compile with tests
sbt test:compile

# Compile and create local executable
# path: rchain/node/target/universal/stage/bin/rnode
sbt stage

# Compile Docker image
sbt docker:publishLocal

# Clean project (except bnfc generated Java code)
sbt clean

# Clean bnfc generated Java code
sbt bnfc:clean
```

### `sbt`  interactive mode

```sh
# Enter sbt interactive mode
sbt

# sbt interactive commands
# sbt:rchain>

# Regenerate parser from bnfc (./rholang/src/main/java)
# - Java code not part of _target_ folder
bnfc:generate

# Compile
compile

# Compile with tests
test:compile

# Compile and create local executable
# path: rchain/node/target/universal/stage/bin/rnode
stage

# Compile Docker image
docker:publishLocal

# Clean project (except bnfc generated Java code)
clean

# Clean bnfc generated Java code
bnfc:clean
```

### Reset Git repository to a clean state

**WARNING: this will remove all non-versioned files from your local repository folder**

```sh
git clean -fdx
```

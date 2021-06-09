## Building and running

__Note__ Successfully building from source requires attending to all of the prerequisites shown below. When users experience errors, it is typically related to failure to assure all prerequisites are met. Work is in progress to improve this experience.

Setup using _nix_ can be found in the [nix](./nix) directory.

### Prerequisites
* Java Development Kit (JDK), version 10. We recommend using the OpenJDK
* [sbt](https://www.scala-sbt.org/download.html)
* For Rholang
     - [jflex](http://jflex.de/)
     - [BNFC](https://github.com/BNFC/bnfc/releases) >= 2.8.3

#### Development environment on macOS

```
brew install git
brew install sbt
brew install jflex
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
cabal update
cabal install bnfc
```

Download and run the installer of the [Haskell Platform](https://www.haskell.org/platform/mac.html#osx)

#### Development environment on Ubuntu and Debian
```
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt-get update
sudo apt-get install sbt

sudo apt-get install jflex
sudo apt-get install haskell-platform
cabal update
cabal install bnfc
```

#### Development environment on Fedora
```
sudo dnf remove sbt # uninstalling sbt if sbt 0.13 was installed (may not be necessary)
sudo dnf --enablerepo=bintray--sbt-rpm install sbt
sudo dnf install jflex
sudo dnf install haskell-platform
cabal update
cabal install bnfc
```

#### Development environment on ArchLinux
You can use `pacaur` or other AUR installer instead of [`trizen`](https://github.com/trizen/trizen).
```
sudo pacman -S stack ghc # for building BNFC
sudo pacman -S jdk8-openjdk sbt
trizen -S jflex
cabal update
cabal install bnfc
```

### Compile all projects and generate executable

Building some of the subprojects is just a matter of `sbt compile`.

```sh
# Compile all projects
sbt:rchain> compile

# Compile all projects with tests
sbt:rchain> test:compile

# Create executable
# path: rchain/node/target/universal/stage/bin/rnode
sbt:rchain> stage

# Delete all files created in build process
sbt:rchain> clean
```

Default memory limits may not be sufficient so additional options for _sbt_ can be specified. They can be added to `.bashrc` file.

Increase heap memory and thread stack size. Disable _supershell_ if empty lines are printed in _sbt_ output.

```sh
export SBT_OPTS="-Xmx4g -Xss2m -Dsbt.supershell=false"
```

### IDE support

For working with the project in **IntelliJ Idea IDE** it's enough to compile the project in terminal and open it in IDE which will start sbt import.

If multiple versions of the JVM are installed on the system, manual selection of the default SDK may be required.

![image](https://user-images.githubusercontent.com/5306205/121373119-1b1dea00-c93f-11eb-83a7-54dcb9fc6c4e.png)

With _Scala (Metals)_ extension and `bloop` compiler, **VSCode editor** can also be used to work on the project.

## Information for developers
Assure prerequisites shown above are met.

### Developer Quick-Start

When working in a single project, scope all `sbt` commands to that project. The most effective way is to maintain a running `sbt` instance, invoked from the project root:
```
$ sbt
[info] Loading settings from plugins.sbt ...
[info] Loading global plugins from /home/kirkwood/.sbt/1.0/plugins
[info] Loading settings from plugins.sbt,protoc.sbt ...
[info] Loading project definition from /home/kirkwood/src/rchain/project
[info] Loading settings from build.sbt ...
[info] Set current project to rchain (in build file:/home/kirkwood/src/rchain/)
[info] sbt server started at local:///home/kirkwood/.sbt/1.0/server/e6a65c30ec6e52272d3a/sock
sbt:rchain> project rspace
[info] Set current project to rspace (in build file:/home/kirkwood/src/rchain/)
sbt:rspace> compile
[... compiling rspace ...]
```
but single-line commands work, too:
```
$ sbt "project rspace" clean compile test
```
or
```
$ sbt rspace/clean rspace/compile rspace/test
```

### Building

The build is organized into several, mostly autonomous projects. These projects may be built (and used!) on their own, or they may be combined together to form the full node package. The build process in any case is contained in and controlled by a single, top-level `build.sbt` file. This process is able to produce several different kinds of artifacts, including JAR files (for Scala) and Docker images.

The most up-to-date code is found in the `dev` branch. This brilliant, cutting-edge source is periodically merged into `master`, which branch should represent a more stable, tested version.

#### Compile only Rholang with parser generator

Inside `rholang`  project is BNFC grammar specification as a source for parser generator. It will run as part of build compile step but it can be run separately, for example to clean and generate parser:  
```
> sbt rholang/clean bnfc:generate

> sbt rholang/clean rchain/compile
```

#### Packaging
To publish a docker image to your local repo run:
```
> sbt node/docker:publishLocal
[... output snipped ...]
[info] Step 8/8 : ENTRYPOINT ["\/bin\/main.sh"]
[info]  ---> Running in 2ac7f835192d
[info] Removing intermediate container 2ac7f835192d
[info]  ---> 5e79e6d92528
[info] Successfully built 5e79e6d92528
[info] Tagging image 5e79e6d92528 with name: coop.rchain/rnode
[success] Total time: 35 s, completed May 24, 2018 10:19:14 AM
```

Check the local docker repo:
```
> docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
coop.rchain/rnode   latest              5e79e6d92528        7 minutes ago       143MB
<none>              <none>              e9b49f497dd7        47 hours ago        143MB
openjdk             8u151-jre-alpine    b1bd879ca9b3        4 months ago        82MB
```

To deploy a tarball run:
```
> sbt node/universal:packageZipTarball
```

The tarball can be found in directory `node/target/universal/`

#### Running
To run rnode locally from within sbt use the revolver plugin. It will start the app in a forked JVM.
```
> sh> sbt
> sbt:rchain> project node
> sbt:node> reStart run -s
```
Now after you've done some local changes and want to test them, simply run the last command `reStart run -s` again. It will kill the running app and start a new instance containing latest changes in a completely new forked JVM.

#### Running tests in IntelliJ

For tests of the Rholang module, make sure you've got the following JVM options set in your Run Configuration:
`-Xss240k -XX:MaxJavaStackTraceDepth=10000 -Xmx128m`

Otherwise the StackSafetySpec is going to be veeery slow and will most likely fail due to timeouts.

You can make the above options default by editing the ScalaTest Template in `Run > Edit configurations > Templates`.  

### Cross-developing for Linux (e.g. Ubuntu) on a Mac
You will need a virtual machine running the appropriate version of Linux.
1. Install [VirtualBox]( https://www.virtualbox.org/wiki/Downloads)
2. Install the Linux distribution you need (e.g. [Ubuntu](http://releases.ubuntu.com/16.04/ubuntu-16.04.4-server-amd64.iso))
3. Start VirtualBox and create a new virtual machine in the manager
4. Boot your virtual machine using the Linux distribution ISO installed in step 2.
5. Configure your Linux VM as desired. You may need to install additional tools sucah as g++, g++-multilib, make, git, etc.

For a more convenient experience, you can share a folder on your Mac with the virtual machine. To do this you will need to install the VirtualBox Guest Additions. Unfortunately there are some gotchas with this. You may need to utilize one of these [solutions](https://askubuntu.com/questions/573596/unable-to-install-guest-additions-cd-image-on-virtual-box).

## Description of subprojects

More info about subprojects can be found in the [Wiki](https://github.com/rchain/rchain/wiki/The-philosophy-of-RChain-node-architecture).

### Communication

The [comm](comm) subproject contains code for network related operations for RChain.

### Rholang

The [rholang](rholang) subproject contains compiler related code for the Rholang language.

### Rspace

The [rspace](rspace) subproject contains code related to the key-value storage of the RChain blockchain.

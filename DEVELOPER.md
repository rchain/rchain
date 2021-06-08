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

#### Building and running
Building some of the subprojects is just a matter of `sbt compile`, however some (like `rholang` or `crypto`) require extra steps to build. See README.md of each subproject for details.

Once you can build each subproject individually, run `sbt node/assembly` to build an executable. The assembled jar will be available under `./node/target/scala-2.12/rnode-assembly-x.y.z.jar`

Example
```scala
sbt:rchain> node/assembly
[info] Including: JLex.jar
[info] Including: log4s_2.12-1.4.0.jar
[info] Including: java-cup-11b-runtime.jar

(...)

[info] SHA-1: bd4471642bb340c8b1fc0571fc614902c5bafbb2
[info] Packaging /Users/rabbit/projects/rchain/node/target/scala-2.12/rnode-assembly-0.1.3.jar ...
[info] Done packaging.
[success] Total time: 25 s, completed Mar 26, 2018 3:36:09 PM
```

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

#### Whole-project Build

Before building the project for the first time you need to generate the `rholang` parser code:
```
> sbt clean bnfc:clean bnfc:generate
```
Please check the prerequistes on your machine if the code generation fails.
Then build the whole project with all submodules:
```
> sbt compile
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
To run RNode locally, first execute:
```
~/rchain$ sbt stage
```
This will build an executable on the path `./node/target/universal/stage/bin/rnode` relative to the repository.

Next, make a directory where you would like to store the logs, keys, and other information related to your rnode testing:\
e.x.
```
~$ mkdir testing
```

Inside this directory, create a new directory named `genesis` and add to `genesis` the files `bonds.txt` and `wallets.txt`:
```
~/testing$ mkdir genesis
~/testing$ cd genesis
~/testing/genesis$ touch wallets.txt
~/testing/genesis$ touch bonds.txt
```

Populate `bonds.txt` with the following information: 
```
04688b8920885b3d41d517a5a5d3088ae4b801938855e5cb215cfc9bb4e857bde1f5c559d57a8332be1308a282b69e542184ee8231e75f8287a3aa8fe91d5293c4 320000000
040ee2bbd611a0d630d04574ba9d4b11b05e84d834b0e918d1445ae0d4e6152cfe1b4ea8daeadd8e212d66ad4bb312d4e236b9b0a187326ea40d17a7d137934f62 750000000
042be28cef3700a6f5d95ec670af061ec69002efee4d553e398b032ea1f5cb9550c262ef06a46f9cdec65660a66d05dd27cbb84e8e714d84822523395ee7cc3790 480000000
```

Populate `wallets.txt` with rev wallet[s] with which you would like to perform testing. These can be generated using the RNode client testing page located at https://tgrospic.github.io/rnode-client-js/. When adding wallets to `wallets.txt`, use the format `public_ethereum_key,rev_balance,0`:\
e.x.
```
~/testing/genesis$ echo "96c44b4cea933e6ebab8cc481a407bc1af99a4ac,10000000000,0" >> wallets.txt
```

Finally, run a standalone RNode using the command: 
```
~$ rchain/node/target/universal/stage/bin/rnode run -s --data-dir testing
```
Add additional flags to this command as necessary. To see a list of flags, run `~$ rchain/node/target/universal/stage/bin/rnode`

To run rnode locally from the sbt shell, run: 
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

### Communication

The [comm](comm) subproject contains code for network related operations for RChain.

### Rholang

The [rholang](rholang) subproject contains compiler related code for the Rholang language.

### Rspace

The [rspace](rspace) subproject contains code related to the key-value storage of the RChain blockchain.


Welcome to Rosette.

    There is more information about Rosette and the code in `README`, but that
    file is of mostly historical interest. Much has been changed.

    Some interesting details are available here:

        https://rchain.atlassian.net/wiki/spaces/ROS/pages/28672088/Overview+of+Rosette+VM

    The documents in docs/ are of interest to those who want to program the
    Rosette VM.


Installing

    Compiled binaries and packages for your OS distribution may be available on
    RChain's developer web site at:

        https://developer.rchain.coop.

Compiling Rosette

    Compile Rosette this way:

        ./build.sh

    If everything works, you'll get a working binary in:

        .../build.out/src/rosette

    Also, you will get a DEB package in:

        .../build.out/rosette-0.1.1-Linux.deb


Dependencies

    Rosette requires of your system a number of basic libraries utilities.  In
    particular, Rosette requires CMake and GFlags. For more details, see the
    CMakelists.txt.

    As of Ubuntu Trusty, the correct GFlags package is:

        https://packages.ubuntu.com/trusty/libgflags2

    Other required tools and libraries:
 
        sudo apt-get install g++
        sudo apt-get install g++-multilib
        sudo apt-get install make
        sudo apt-get install cmake
        sudo apt-get install clang

    Rosette requires 32-bit c++ Google protobufs:

        To get the source, download one of the release .tar.gz or .zip packages in the release page.

        For example:

        https://github.com/google/protobuf/releases/download/v3.5.1/protobuf-cpp-3.5.1.tar.gz

        For example, download protobuf-cpp-[VERSION].tar.gz and extract it into a directory on your system.

        To build and install the C++ Protocol Buffer runtime and the Protocol Buffer compiler (protoc),
        cd into the directory where you extracted the above .tar.gz and execute the following:

        $ ./configure --build=i686-pc-linux-gnu CFLAGS="-m32 -DNDEBUG" CXXFLAGS="-m32 -DNDEBUG" LDFLAGS=-m32
        $ make
        $ make check
        $ sudo make install
        $ sudo ldconfig # refresh shared library cache.

    Optional tools and libraries:

        sudo apt-get install doxygen

	Doxygen requires dot from http://www.graphviz.org/download/


Notes on development.

    1. CMakeLists.txt enables all warnings, but Rosette is old, and some
    warnings have to be suppressed. Do not suppress any new compiler warnings.


Style

    Generally, you should use `clang-format` and the configuration file that's
    present in the root directory.

    Editor settings should mirror the following behavior from vim:

        set tabstop=4
        set shiftwidth=4
        set softtabstop=4
        set expandtab



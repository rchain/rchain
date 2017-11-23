# Packages for Rosette-VM building in travis
# See also: ../.travis.yml, ../scripts/install.sh
if [[ "$TRAVIS_OS_NAME" == "linux" ]; then
   sudo apt install g++ g++-multilib
fi

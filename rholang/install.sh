# BNFC install for travis
# Ubuntu 14.04 has an outdated BNFC package, so we build BNFC from source
#
# see also: ../.travis.yml, ../scripts/install.sh
if [[ "$TRAVIS_OS_NAME" == "linux" ]; then
       sudo apt install haskell-platform
fi
if [[ "$TRAVIS_OS_NAME" == "osx" ]; then
       brew install haskell-platform
fi
git clone https://github.com/BNFC/bnfc.git
cd bnfc/source
sudo cabal install --global

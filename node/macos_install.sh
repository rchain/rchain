#!/usr/bin/env bash

# Check if Homebrew is installed, install if we don't have it, update if we do
homebrew() {
  if [[ $(command -v brew) == "" ]]; then
    echo "Installing Homebrew.. "
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  else
    echo "Updating Homebrew.. "
    brew update
  fi
}


# RUN
homebrew
brew install libsodium

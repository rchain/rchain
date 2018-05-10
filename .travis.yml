sudo: required
dist: trusty

matrix:
  include:
  - language: nix
    env: SUBPROJECT=rosette
  - language: scala
    env: SUBPROJECT=core
    scala: 2.12.4
    sbt_args: -no-colors
    install:
      - ./scripts/install_secp.sh
      - ./scripts/install_sodium.sh
      - ./scripts/install.sh
    addons:
      apt:
        sources:
          - sourceline: 'deb https://dl.bintray.com/sbt/debian /'
        packages:
          - sbt
          - jflex
          - haskell-platform
          - rpm
          - fakeroot
  - language: scala
    env: SUBPROJECT=test_artifact_creation
    scala: 2.12.4
    sbt_args: -no-colors
    install:
      - ./scripts/install_secp.sh
      - ./scripts/install_sodium.sh
      - ./scripts/install.sh
    addons:
      apt:
        sources:
          - sourceline: 'deb https://dl.bintray.com/sbt/debian /'
        packages:
          - sbt
          - jflex
          - haskell-platform
          - rpm
          - fakeroot
  - language: scala
    env: SUBPROJECT=rnode-dockerhub-push
    scala: 2.12.4
    sbt_args: -no-colors
    install:
      - ./scripts/install_secp.sh
      - ./scripts/install_sodium.sh
      - ./scripts/install_bnfc.sh
      - ./scripts/rnode-dockerhub-push.sh
    addons:
      apt:
        sources:
          - sourceline: 'deb https://dl.bintray.com/sbt/debian /'
        packages:
          - sbt
          - jflex
          - haskell-platform
          - rpm
          - fakeroot
  - language: scala
    env: SUBPROJECT=p2p-test-network
    scala: 2.12.4
    sbt_args: -no-colors
    install:
      - ./scripts/install_secp.sh
      - ./scripts/install_sodium.sh
      - ./scripts/install_bnfc.sh
    addons:
      apt:
        sources:
          - sourceline: 'deb https://dl.bintray.com/sbt/debian /'
        packages:
          - sbt
          - jflex
          - haskell-platform
          - rpm
          - fakeroot
  - language: scala
    env: SUBPROJECT=cloud-p2p-test-network
    scala: 2.12.4
    sbt_args: -no-colors
    install:
      - ./scripts/install_secp.sh
      - ./scripts/install_sodium.sh
      - ./scripts/install_bnfc.sh
    addons:
      apt:
        sources:
          - sourceline: 'deb https://dl.bintray.com/sbt/debian /'
        packages:
          - sbt
          - jflex
          - haskell-platform
          - rpm
          - fakeroot
  - language: nix 
    env: SUBPROJECT=rholang_more_tests

script:
  - ./scripts/build-subprojects.sh

cache:
  directories:
    - "$HOME/.ivy2/cache"
    - "$HOME/.sbt"

after_success:
  - "./scripts/create-artifacts.sh"

deploy:
  provider: releases
  api_key: "$GITHUB_RELEASES_API_TOKEN_PUBLIC_REPO"
  file_glob: true
  file:
    - "node/target/*.deb"
    - "node/target/rpm/RPMS/noarch/*.rpm"
    - "node/target/universal/*.tgz"
  skip_cleanup: true
  on:
    tags: true
    branch: master

notifications:
  email:
    recipients: rchain-makers@pyrofex.net
    on_success: never
    on_failure: always

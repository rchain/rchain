import sbt.{taskKey, File, IO, URL}

object Secp256k1 {

  lazy val linux_x86_x64_local = "secp256k1/src/main/resources/secp256k1-native-linux-x86_64.so"
  lazy val osx_x86_x64_local   = "secp256k1/src/main/resources/secp256k1-native-osx-x86_64.dylib"
  lazy val osx_aarch64_local   = "secp256k1/src/main/resources/secp256k1-native-osx-aarch64.dylib"

  lazy val linux_x86_x64_remote =
    "https://github.com/nzpr/secp256k1-native/raw/aarch64/libs/secp256k1-native-linux-x86_64.so"
  lazy val osx_x86_x64_remote =
    "https://github.com/nzpr/secp256k1-native/raw/aarch64/libs/secp256k1-native-osx-x86_64.dylib"
  lazy val osx_aarch64_remote =
    "https://github.com/nzpr/secp256k1-native/raw/aarch64/libs/secp256k1-native-osx-aarch64.dylib"

  lazy val pullNativeLibs =
    taskKey[Unit]("Download native libraries and place them into secp256k1 project")
}

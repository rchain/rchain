import sbt._
import Keys._

import scala.sys.process._

object Secp256k1 {

  lazy val Secp256k1Config = config("secp256k1")
  lazy val generate = taskKey[Unit]("Make sources")
  lazy val secp256kOutputDir = settingKey[File]("Directory containing native library.")

  //Clean shoud remove all directories created during the build process
  //even better - it should reset the project to the HEAD state
  //  git fetch origin
  //  git reset --hard origin/master
  //remove *.so *.dll and *.dylib files from resources folder
  def cleanDirs(dirs: File*): Unit = dirs.foreach(dir => s"rm -rf $dir".!)

  def cleanProject(projectDir: File, dirs: File*) = {
    cleanDirs(dirs:_*)
    val makeDistCleanCmd = s"make -C ${projectDir.getAbsolutePath} distclean"
    makeDistCleanCmd.!
  }

  def secp256k1Generate(baseDir: File) = {
    val autogenCmd: String = "./autogen.sh"
    val autogenProcess = Process(autogenCmd, new File(s"${baseDir.getAbsolutePath}/secp256k1/"))
    val configureCmd: String = "./configure --enable-jni --enable-experimental --enable-module-ecdh"
    val configureProcess = Process(configureCmd, new File(s"${baseDir.getAbsolutePath}/secp256k1/"))
    val makeCmd: String = s"make -C ${baseDir.getAbsolutePath}/secp256k1"
    val makeProcess = Process(makeCmd)
    val binariesSourceFolder = baseDir / "secp256k1" / ".libs"
    val resourcesFolder = baseDir / "src" / "main" / "resources"
    val makeResourcesFolderCmd = s"mkdir ${resourcesFolder.getAbsolutePath}"
    val mkResourcesFolderProcess = Process(makeResourcesFolderCmd)
    val copyBinariesCmd: String = s"cp libsecp256k1.so ${resourcesFolder.getAbsolutePath}/"
    val copyBinariesProcess = Process(copyBinariesCmd, binariesSourceFolder.getAbsoluteFile)
    val status = autogenProcess #&& configureProcess #&& makeProcess #&& mkResourcesFolderProcess  #&& copyBinariesProcess !

    if (status != 0) {
      cleanProject(baseDir / "secp256k1", resourcesFolder)
    } else {
      status
    }
  }

  lazy val secp256k1Settings = inConfig(Secp256k1Config)(Defaults.configSettings ++ Seq(
    secp256kOutputDir := baseDirectory.value / "src" / "main" / "resources",
    clean := cleanProject(baseDirectory.value / "secp256k1", Seq(secp256kOutputDir.value, baseDirectory.value / "secp256k1"/ ".libs"):_*),
    generate := secp256k1Generate(baseDirectory.value)
  ))

}

import sbt._
import Keys._

/*
 * There doesn't seem to be a good way to have a plugin defined in 
 * one sub-project be used in another sub-project. So we need to
 * call an assembled jar instead, building it if necessary.
 */
object Rholang {

  def jarOutDated(jar: File, jarSources: File): Boolean = {
    val scalaFiles = (jarSources ** "*.scala").get
    val latestSource = scalaFiles.maxBy(_.lastModified())
    
    (!jar.exists()) || (jar.lastModified() < latestSource.lastModified())
  }

  def constructArtifacts(jar: File, rhoSourceDir: File, outputDir: File): Seq[File] = {
    import scala.sys.process._

    val command = 
      s"java -jar ${jar.getAbsolutePath()} ${rhoSourceDir.getAbsolutePath()} ${outputDir.getAbsolutePath()}"
      
    val status = command.!
    
    if (status == 0) {
      (outputDir ** "*.scala").get
    } else {
      throw new Exception(s"Non-zero exit status during rholang artifact creation: $status")
    }
  }
}

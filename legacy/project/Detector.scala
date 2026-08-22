import java.io.File
import java.util.{Locale, Properties}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

/*
  OS detector, inspired by the Maven plug-in
  https://github.com/trustin/os-maven-plugin
 */
object Detector {
  private val Unknown                 = "unknown"
  private val LinuxIdPrefix           = "ID="
  private val LinuxIdLikePrefix       = "ID_LIKE="
  private val LinuxVersionIdPrefix    = "VERSION_ID="
  private val LinuxOsReleaseFiles     = Seq("/etc/os-release", "/usr/lib/os-release")
  private val RedhatReleaseFile       = "/etc/redhat-release"
  private val DefaultRedhatVariants   = Seq("rhel", "fedora")
  private val VersionRegex            = raw"((\d+)\.(\d+)).*".r
  private val RedhatMajorVersionRegex = raw"(\d+)".r

  private def normalizeOs(value: String): String =
    normalize(value) match {
      case v if v.startsWith("aix")  => "aix"
      case v if v.startsWith("hpux") => "hpux"
      case v
          if v.startsWith("os400")
            && (v.length <= 5 || !Character.isDigit(value.charAt(5))) =>
        "os400"
      case v if v.startsWith("linux")                                => "linux"
      case v if v.startsWith("macosx") || value.startsWith("osx")    => "osx"
      case v if v.startsWith("freebsd")                              => "freebsd"
      case v if v.startsWith("openbsd")                              => "openbsd"
      case v if v.startsWith("netbsd")                               => "netbsd"
      case v if v.startsWith("solaris") || value.startsWith("sunos") => "sunos"
      case v if v.startsWith("windows")                              => "windows"
      case _                                                         => Unknown
    }

  private def normalizeArch(value: String): String =
    normalize(value) match {
      case v if v.matches("^(x8664|amd64|ia32e|em64t|x64)$") => "x86_64"
      case v if v.matches("^(x8632|x86|i[3-6]86|ia32|x32)$") => "x86_32"
      case v if v.matches("^(ia64w?|itanium64)$")            => "itanium_64"
      case "ia64n"                                           => "itanium_32"
      case v if v.matches("^(sparc|sparc32)$")               => "sparc_32"
      case v if v.matches("^(sparcv9|sparc64)$")             => "sparc_64"
      case v if v.matches("^(arm|arm32)$")                   => "arm_32"
      case "aarch64"                                         => "aarch64"
      case v if v.matches("^(mips|mips32)$")                 => "mips_32"
      case v if v.matches("^(mipsel|mips32el)$")             => "mipsel_32"
      case "mips64"                                          => "mips_64"
      case "mips64el"                                        => "mipsel_64"
      case v if v.matches("^(ppc|ppc32)$")                   => "ppc_32"
      case v if v.matches("^(ppcle|ppc32le)$")               => "ppcle_32"
      case "ppc64"                                           => "ppc_64"
      case "ppc64le"                                         => "ppcle_64"
      case "s390"                                            => "s390_32"
      case "s390x"                                           => "s390_64"
      case _                                                 => Unknown
    }

  private def normalize(value: String): String =
    Option(value)
      .map(_.toLowerCase(Locale.US).replaceAll("[^a-z0-9]+", ""))
      .getOrElse("")

  private def getLinuxRelease: Option[LinuxRelease] =
    LinuxOsReleaseFiles
      .map(f => new File(f) -> parseLinuxOsReleaseFile _)
      .:+(new File(RedhatReleaseFile) -> parseLinuxRedhatReleaseFile _)
      .foldLeft(Option.empty[LinuxRelease]) {
        case (lr, _) if lr.isDefined       => lr
        case (_, (f, parse)) if f.exists() => parse(f)
        case (lr, _)                       => lr
      }

  private def parseLinuxOsReleaseFile(file: File): Option[LinuxRelease] = {

    @tailrec
    def loopLines(
        ls: List[String],
        id: Option[String],
        version: Option[String],
        likeSet: Set[String]
    ): Option[LinuxRelease] =
      ls match {
        case Nil =>
          id.map(LinuxRelease(_, version, likeSet))

        case l :: tail if l.startsWith(LinuxIdPrefix) =>
          val str = normalizeOsReleaseValue(l.substring(LinuxIdPrefix.length))
          loopLines(tail, Some(str), version, likeSet + str)

        case l :: tail if l.startsWith(LinuxVersionIdPrefix) =>
          val str = normalizeOsReleaseValue(l.substring(LinuxVersionIdPrefix.length))
          loopLines(tail, id, Some(str), likeSet)

        case l :: tail if l.startsWith(LinuxIdLikePrefix) =>
          val str   = normalizeOsReleaseValue(l.substring(LinuxIdLikePrefix.length))
          val parts = str.split("\\s+")
          loopLines(tail, id, version, likeSet ++ parts)

        case _ :: tail => loopLines(tail, id, version, likeSet)
      }

    val source = Source.fromFile(file)
    val lines  = Try(source.getLines().toList).getOrElse(Nil)
    source.close()
    loopLines(lines, None, None, Set())
  }

  private def parseLinuxRedhatReleaseFile(file: File): Option[LinuxRelease] = {
    val source = Source.fromFile(file)
    val line   = Try(source.getLines().toList.headOption.map(_.toLowerCase())).getOrElse(None)
    source.close()

    val id = line.flatMap {
      case l if l.contains("centos")                   => Some("centos")
      case l if l.contains("fedora")                   => Some("fedora")
      case l if l.contains("red hat enterprise linux") => Some("rhel")
      case _                                           => None
    }

    id.map { i =>
      val version = line.flatMap {
        case RedhatMajorVersionRegex(v) => Some(v)
        case _                          => None
      }

      val likeSet = Set(DefaultRedhatVariants: _*) + i
      LinuxRelease(i, version, likeSet)
    }
  }

  private def normalizeOsReleaseValue(value: String): String = // Remove any quotes from the string.
    value.trim.replace("\"", "")

  def detect(classifierWithLikes: Seq[String] = Nil): DetectedProperties = {
    val props        = new Properties(System.getProperties)
    val osName       = props.getProperty("os.name")
    val osArch       = props.getProperty("os.arch")
    val osVersion    = props.getProperty("os.version")
    val detectedName = Detector.normalizeOs(osName)
    val detectedArch = Detector.normalizeArch(osArch)
    val linuxRelease =
      if (detectedName == "linux")
        Detector.getLinuxRelease
      else
        None

    val (version, major, minor) = osVersion match {
      case VersionRegex(v, ma, mi) =>
        (Some(v), Some(ma), Some(mi))
      case _ => (None, None, None)
    }

    val detectedClassifierBuilder = new StringBuilder
    detectedClassifierBuilder.append(detectedName)
    detectedClassifierBuilder.append('-')
    detectedClassifierBuilder.append(detectedArch)
    linuxRelease.foreach { lr =>
      classifierWithLikes.find(lr.like.contains).foreach { classifierLike =>
        detectedClassifierBuilder.append('-')
        detectedClassifierBuilder.append(classifierLike)
      }
    }

    DetectedProperties(
      detectedName,
      detectedArch,
      detectedClassifierBuilder.toString,
      version,
      major,
      minor,
      linuxRelease.map(_.id),
      linuxRelease.flatMap(_.version)
    )
  }
}

private case class LinuxRelease(id: String, version: Option[String], like: Set[String])

case class DetectedProperties(
    osName: String,
    osArch: String,
    osClassifier: String,
    osVersion: Option[String],
    osVersionMajor: Option[String],
    osVersionMinor: Option[String],
    linuxRelease: Option[String],
    linuxReleaseVersion: Option[String]
)

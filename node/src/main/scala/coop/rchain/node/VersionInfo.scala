package coop.rchain.node

object VersionInfo {
  val get: String = s"RChain Node ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})"
}

package coop.rchain.comm.transport

import java.nio.file.Path

final case class TlsConf(
    certificatePath: Path,
    keyPath: Path,
    secureRandomNonBlocking: Boolean,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean
)

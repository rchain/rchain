package coop.rchain.comm.transport

import java.nio.file.Path

final case class Tls(
    certificate: Path,
    key: Path,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean,
    secureRandomNonBlocking: Boolean
)

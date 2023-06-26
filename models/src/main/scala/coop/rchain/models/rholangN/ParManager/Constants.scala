package coop.rchain.models.rholangN.ParManager

import coop.rchain.rspace.hashing.Blake2b256Hash

private[ParManager] object Constants {
  final val intSize     = 4
  final val longSize    = 8
  final val booleanSize = 1
  final val hashSize    = Blake2b256Hash.length

  final val tagSize = 1

  /** Tags for serialization */
  /** Main pars */
  final val PARPROC: Byte = 0x01.toByte
  final val SEND          = 0x02.toByte
  final val RECEIVE       = 0x03.toByte
  final val MATCH         = 0x04.toByte
  final val NEW           = 0x05.toByte

  /** Ground types */
  final val GNIL     = 0x10.toByte
  final val GBOOL    = 0x11.toByte
  final val GINT     = 0x12.toByte
  final val GBIG_INT = 0x13.toByte
  //    final val GSTRING = 0x14.toByte
  //    final val GURI = 0x15.toByte
  //    final val GPRIVATE = 0x16.toByte

  /** Collections */
  final val ELIST = 0x17.toByte
  //    final val ETUPLE = 0x18.toByte
  //    final val ESET = 0x19.toByte
  //    final val EMAP = 0x1A.toByte

  /** Vars */
  final val BOUND_VAR = 0x30.toByte
  final val FREE_VAR  = 0x31.toByte
  final val WILDCARD  = 0x32.toByte

  /** Expr */
  //    final val EVAR        = 0x40.toByte
  //    final val ENEG        = 0x41.toByte
  //    final val EMULT       = 0x42.toByte
  //    final val EDIV        = 0x43.toByte
  //    final val EPLUS       = 0x44.toByte
  //    final val EMINUS      = 0x45.toByte
  //    final val ELT         = 0x56.toByte
  //    final val ELTE        = 0x47.toByte
  //    final val EGT         = 0x48.toByte
  //    final val EGTE        = 0x49.toByte
  //    final val EEQ         = 0x4A.toByte
  //    final val ENEQ        = 0x4B.toByte
  //    final val ENOT        = 0x4C.toByte
  //    final val EAND        = 0x4E.toByte
  //    final val EOR         = 0x4F.toByte
  //    final val EMETHOD     = 0x50.toByte
  //    final val EBYTEARR    = 0x51.toByte
  //    final val EEVAL       = 0x52.toByte
  //    final val EMATCHES    = 0x53.toByte
  //    final val EPERCENT    = 0x54.toByte
  //    final val EPLUSPLUS   = 0x55.toByte
  //    final val EMINUSMINUS = 0x56.toByte
  //    final val EMOD        = 0x57.toByte
  //    final val ESHORTAND   = 0x58.toByte
  //    final val ESHORTOR    = 0x59.toByte

  /** Bundle */
  //    final val BUNDLE_EQUIV      = 0x60.toByte
  //    final val BUNDLE_READ       = 0x61.toByte
  //    final val BUNDLE_WRITE      = 0x62.toByte
  //    final val BUNDLE_READ_WRITE = 0x63.toByte

  /** Connective */
  //    final val CONNECTIVE_NOT       = 0x71.toByte
  //    final val CONNECTIVE_AND       = 0x72.toByte
  //    final val CONNECTIVE_OR        = 0x73.toByte
  //    final val CONNECTIVE_VARREF    = 0x74.toByte
  //    final val CONNECTIVE_BOOL      = 0x75.toByte
  //    final val CONNECTIVE_INT       = 0x76.toByte
  //    final val CONNECTIVE_STRING    = 0x77.toByte
  //    final val CONNECTIVE_URI       = 0x78.toByte
  //    final val CONNECTIVE_BYTEARRAY = 0x79.toByte
  //    final val CONNECTIVE_BIG_INT   = 0x7A.toByte

  /** Auxiliary types */
  final val RECEIVE_BIND = 0x80.toByte
  final val MATCH_CASE   = 0x81.toByte
}

package coop.rchain.models.rholangN.ParManager

import coop.rchain.rspace.hashing.Blake2b256Hash

private[ParManager] object Constants {
  final val intSize     = 4
  final val longSize    = 8
  final val booleanSize = 1
  final val hashSize    = Blake2b256Hash.length

  final val tagSize = 1

  /** Tags for serialization */
  /** Basic types */
  final val PARPROC = 0x01.toByte
  final val SEND    = 0x02.toByte
  final val RECEIVE = 0x03.toByte
  final val MATCH   = 0x04.toByte
  final val NEW     = 0x05.toByte

  /** Ground types */
  final val GNIL        = 0x10.toByte
  final val GBOOL       = 0x11.toByte
  final val GINT        = 0x12.toByte
  final val GBIG_INT    = 0x13.toByte
  final val GSTRING     = 0x14.toByte
  final val GBYTE_ARRAY = 0x15.toByte
  final val GURI        = 0x16.toByte

  /** Collections */
  final val ELIST  = 0x20.toByte
  final val ETUPLE = 0x21.toByte
  //    final val ESET = 0x22.toByte
  //    final val EMAP = 0x23.toByte

  /** Vars */
  final val BOUND_VAR = 0x2A.toByte
  final val FREE_VAR  = 0x2B.toByte
  final val WILDCARD  = 0x2C.toByte

  /** Unforgeable names */
  final val UPRIVATE     = 0x30.toByte
  final val UDEPLOY_ID   = 0x31.toByte
  final val UDEPLOYER_ID = 0x32.toByte

  /** Operations */
  final val ENEG = 0x40.toByte
  final val ENOT = 0x41.toByte

  final val EPLUS       = 0x42.toByte
  final val EMINUS      = 0x43.toByte
  final val EMULT       = 0x44.toByte
  final val EDIV        = 0x45.toByte
  final val EMOD        = 0x46.toByte
  final val ELT         = 0x47.toByte
  final val ELTE        = 0x48.toByte
  final val EGT         = 0x49.toByte
  final val EGTE        = 0x4A.toByte
  final val EEQ         = 0x4B.toByte
  final val ENEQ        = 0x4C.toByte
  final val EAND        = 0x4D.toByte
  final val ESHORTAND   = 0x4E.toByte
  final val EOR         = 0x4F.toByte
  final val ESHORTOR    = 0x50.toByte
  final val EPLUSPLUS   = 0x51.toByte
  final val EMINUSMINUS = 0x52.toByte
  final val EPERCENT    = 0x53.toByte

  final val EMETHOD  = 0x5A.toByte
  final val EMATCHES = 0x5B.toByte

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

  /** Other types */
  final val BUNDLE         = 0x90.toByte
  final val SYS_AUTH_TOKEN = 0x91.toByte
}

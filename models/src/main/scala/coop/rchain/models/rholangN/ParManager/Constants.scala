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
  final val NIL     = 0x01.toByte
  final val PARPROC = 0x02.toByte
  final val SEND    = 0x03.toByte
  final val RECEIVE = 0x04.toByte
  final val MATCH   = 0x05.toByte
  final val NEW     = 0x06.toByte

  /** Ground types */
  final val GBOOL       = 0x10.toByte
  final val GINT        = 0x11.toByte
  final val GBIG_INT    = 0x12.toByte
  final val GSTRING     = 0x13.toByte
  final val GBYTE_ARRAY = 0x14.toByte
  final val GURI        = 0x15.toByte

  /** Collections */
  final val ELIST  = 0x20.toByte
  final val ETUPLE = 0x21.toByte
  final val ESET   = 0x22.toByte
  final val EMAP   = 0x23.toByte

  /** Vars */
  final val BOUND_VAR = 0x2A.toByte
  final val FREE_VAR  = 0x2B.toByte
  final val WILDCARD  = 0x2C.toByte

  /** Operations */
  final val ENEG = 0x30.toByte
  final val ENOT = 0x31.toByte

  final val EPLUS       = 0x32.toByte
  final val EMINUS      = 0x33.toByte
  final val EMULT       = 0x34.toByte
  final val EDIV        = 0x35.toByte
  final val EMOD        = 0x36.toByte
  final val ELT         = 0x37.toByte
  final val ELTE        = 0x38.toByte
  final val EGT         = 0x39.toByte
  final val EGTE        = 0x3A.toByte
  final val EEQ         = 0x3B.toByte
  final val ENEQ        = 0x3C.toByte
  final val EAND        = 0x3D.toByte
  final val ESHORTAND   = 0x3E.toByte
  final val EOR         = 0x3F.toByte
  final val ESHORTOR    = 0x40.toByte
  final val EPLUSPLUS   = 0x41.toByte
  final val EMINUSMINUS = 0x42.toByte
  final val EPERCENT    = 0x43.toByte

  final val EMETHOD  = 0x4A.toByte
  final val EMATCHES = 0x4B.toByte

  /** Unforgeable names */
  final val UPRIVATE     = 0x50.toByte
  final val UDEPLOY_ID   = 0x51.toByte
  final val UDEPLOYER_ID = 0x52.toByte

  /** Connective */
  final val CONNECTIVE_BOOL      = 0x70.toByte
  final val CONNECTIVE_INT       = 0x71.toByte
  final val CONNECTIVE_STRING    = 0x72.toByte
  final val CONNECTIVE_URI       = 0x73.toByte
  final val CONNECTIVE_BYTEARRAY = 0x74.toByte
  final val CONNECTIVE_BIG_INT   = 0x75.toByte
  final val CONNECTIVE_NOT       = 0x76.toByte
  final val CONNECTIVE_AND       = 0x77.toByte
  final val CONNECTIVE_OR        = 0x78.toByte
  final val CONNECTIVE_VARREF    = 0x79.toByte

  /** Auxiliary types */
  final val RECEIVE_BIND = 0x80.toByte
  final val MATCH_CASE   = 0x81.toByte

  /** Other types */
  final val BUNDLE         = 0x90.toByte
  final val SYS_AUTH_TOKEN = 0x91.toByte
}

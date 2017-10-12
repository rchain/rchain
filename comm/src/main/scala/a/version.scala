package a

import org.zeromq.ZMQ

object version {

  def main(args: Array[String]) {
    printf("Version string: %s, Version int: %d\n", ZMQ.getVersionString, ZMQ.getFullVersion)
  }
}


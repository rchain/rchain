package com

package object revdefine {

  val FOLDER_NAME       = "revdefine"
  val MONGO_ENV_URI_KEY = "MONGO_URI"
  val MONGO_URI: String = sys.env.getOrElse(MONGO_ENV_URI_KEY, "mongodb://127.0.0.1:27017")

}

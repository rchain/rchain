package coop.rchain.node.diagnostics

object TagSetToMap {
    def tagSetToMap(tagSet: kamon.tag.TagSet): Map[String, String] = {
        if(tagSet.nonEmpty()) {
            tagSet.all().map((tag) => (tag.key, kamon.tag.Tag.unwrapValue(tag).asInstanceOf[String])).toMap
        }
        else{
            Map.empty
        }
    }
}
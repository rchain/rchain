package coop.rchain.roscala.util

object misc {
  def numberSuffix(n: Int): String = n match {
    case 1 => "st"
    case 2 => "nd"
    case 3 => "rd"
    case _ => "th"
  }

  def properPrep(s: String): String = {
    def isVowel = Set('a', 'e', 'i', 'o', 'u')
    s.charAt(0).toLower match {
      case c if isVowel(c) => "an"
      case _               => "a"
    }
  }
}

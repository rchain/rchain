// -*- mode: Scala;-*- 
// Filename:    search.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 09:12:45 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.zipper

import scala.collection.mutable.Map

trait ZipperMap[A]
extends Map[Location[A],Tree[A]] {
}

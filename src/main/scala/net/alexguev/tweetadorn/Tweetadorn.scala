package net.alexguev.tweetadorn

import scala.xml.NodeSeq
class Tweetadorn(author: String) {

  def run = {
    val interestingChanges = changes filter { change => new Page(change.url).createdBy == Some(author) }
    print(interestingChanges)
  }

  def changes: Seq[Change] = {
    return for {
      divContent <- (PageLoader.loadFromUrl("http://i-proving.ca/space/snipsnap-index/Recent+Changes") \\ "DIV")
      if ((divContent \ "@class").text == "snip-content")
      divList <- divContent \\ "DIV"
      if ((divList \ "@class").text == "list")
      li <- divList \ "UL" \ "LI"
    } yield new Change(li)
  }

  def print(changes: Seq[Change]) {
    println((for (change <- changes; url <- change.url) yield url).foldLeft("<<<END>>>"){(change1, change2) => change1 + "\n" + change2 })
  }

  var a = 0;
  class Change(val liContent: NodeSeq) {
    def url: Option[String] = {
      (liContent \ "A").zipWithIndex 
      	.find { case (_, index) => index == 0 }
      	.filter { case(node, _) => (node \ "@class" text) == "iconComment"} 
      	.map { case(node, _) => node \ "@href" text} 
      	.map { "http://i-proving.ca/" + _ }
    }
  }

  class Page(val url: Option[String]) {
    def createdBy: Option[String] = {
      url match {
        case Some(url) => {
          val page = PageLoader.loadFromUrl(url)
          val contentInfoNode = page \\ "DIV" find { node => (node \ "@id").text == "ricardo-content-info" }
          contentInfoNode match {
            case Some(div) => 
            	(div \ "A").zipWithIndex find { case (_, index) => index == 1 } match {
            		case Some((node, _)) => Some(node text)
            		case None => None
            	}
            case None => None
          }
        }
        case None => None 
      }
    }
    	
  }

}

object Tweetadorn {

  def main(args: Array[String]) {
    new Tweetadorn("Alexei Guevara") run
  }
}
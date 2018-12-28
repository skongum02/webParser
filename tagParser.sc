sealed trait CaptureState

case class Skip() extends CaptureState
case class Capture() extends CaptureState
case class TagX(value: String) extends CaptureState
case class Information(value: String) extends CaptureState

import scala.io.Source

def readFile(f: String): String = {
  Source.fromFile(f).getLines.mkString
}


def parse(str: String, capTag: String, state: CaptureState, tag: String, values: Vector[String]): Vector[String] = {
    if (str.isEmpty) {
       values
    }
    else {
      state match {
        case Skip() if str.head == '<' =>
             parse(str.tail, capTag, Capture(), "", values)
        case Capture() if str.head == '>' =>
            parse(str.tail, capTag, TagX(tag), "", values)
        case TagX(i) if str.head == '<' =>
            if (i == capTag) {
	       println("Found a tag")
               parse(str.tail, capTag, Information(tag), "", values :+ tag)
             }
             else {
               parse(str.tail, capTag, Capture(), "", values)
             }
        case Information(i) =>
          parse(str.tail, capTag, Skip(), "", values)
        case _ =>
             parse(str.tail, capTag, state, tag + str.head, values)

      }
   }
}


abstract class JSON

case class JSeq(elems: List[JSON]) extends JSON
case class JObj(bindings: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON

object Json {

  def show(json: JSON): String = json match {
    // skip other types of JSON
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => "\"" + key + "\":" + show(value)
      }
      "{" + (assocs mkString ", ") + "}"
  }
}
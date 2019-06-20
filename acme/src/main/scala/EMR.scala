package acme

import scala.io.Source.stdin

object EMR {

  import acme.types.Imports._

  def interactIO(f: String => Option[String]): Unit =
    stdin.getLines.foreach { x =>
      f(x).foreach(println)
    }

  // Attempt to decode a `String` representing a single line of input
  def decodeDelimited[A:FromRecord](delim: Char, s: String): Option[A] =
    s.split(delim).map(_.trim.filterNot(_ == '"')).toVector.fromRecord[A]

  // Encode a value to a 'String' using its 'Csv.ToRecord' instance
  def encodeDelimited[A:ToRecord](delim: Char, a: A): String =
    a.toRecord.map("\"" + _ + "\"").mkString(delim.toString)

  val keyDelimiter: Char = '\t'

  // Output a value prefixed by a key
  def encodeKeyed[A](keyEnc: A => String, vEnc: A => String, v: A): String =
    keyEnc(v) + keyDelimiter + vEnc(v)

  def decodeKeyed[K,V](kDec: String => Option[K],
                       vDec: String => Option[V],
                       mKey: String, mVal: String): Option[(K,V)] = for {
    k <- kDec(mKey)
    v <- vDec(mVal)
  } yield (k,v)

  // Group the values in a list of (k, v) pairs together
  def group[K,V](kvs: Iterable[(K,V)]): Iterable[(K,Vector[V])] =
    kvs.foldRight(List[(K,Vector[V])]()) {
      case ((k, v), (pk, pv) +: xs) if k == pk => (pk, v +: pv) :: xs
      case ((k, v), vect) => (k, Vector(v)) :: vect
    }.toIterable

  // Break strings into keys and values.
  // This method expects input of the form `k|v`
  // It calls `kDec(k)` and `vDec(v)` to generate the key/value pair.
  def decode[K,V](kDec: String => Option[K],
                  vDec: String => Option[V],
                  data: Iterable[String]): Iterable[(K,V)] =
    data.map(x => x.split(keyDelimiter) match {
      case Array() => ("","")
      case a => (a.head, a.tail.mkString(""))
    }).flatMap { p =>
      decodeKeyed[K,V](kDec, vDec, p._1, p._2).toIterable
    }

  // Read in all of stdin as an Iterable of Strings
  def lines: Iterable[String] = stdin.getLines.toIterable
}

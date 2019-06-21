package acme.reducer

object Main {

  import acme.types.Imports._
  import acme.EMR

  def main(args: Array[String]): Unit = {

    val groupIds: Iterable[String] = EMR.lines

    groupIds
      .groupBy(identity)
      .map{ case (id, xs) => (id, xs.size) }
      .foreach { case (groupId, number) => println(s"$groupId\t$number") }

  }
}

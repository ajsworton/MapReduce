package acme.mapper

import acme.EMR
import acme.types.Imports.AcmeCustomerAttributes

object Main {

  def main(args: Array[String]): Unit = {

    val lines: Iterable[String] = EMR.lines

    val records: Iterable[Option[AcmeCustomerAttributes]] = lines.map { line =>
      AcmeCustomerAttributes.attrsFromRecord(
        line
          .filterNot(_ == '"')
          .split("\t")
          .toVector
      )
    }

    val filterAge: AcmeCustomerAttributes => Boolean = record => record.age >= 18 && record.age <= 32
    val filterChurn: AcmeCustomerAttributes => Boolean = record => record.churnScore >= 0.95

    val filteredRecordsSegmentId: Iterable[Int] = records
      .collect {
        case Some(record) if filterAge(record) && filterChurn(record) => record.segmentId
      }

    filteredRecordsSegmentId.foreach(println)

  }


}


package acme.mapper

import acme.EMR
import acme.types.Imports.AcmeCustomerAttributes

object Main {

  def main(args: Array[String]): Unit = {

    //    CustomerId  FirstName   LastName        Age   LTVEstimate ChurnScore  SegmentId
    //    "59028934"  "Melanie"   "Gonzalez-Meza" "37"  "2798"      "0.273"     "8"

    //get values
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

    val filteredRecords: Iterable[AcmeCustomerAttributes] = records
      .collect {
        case Some(record) if filterAge(record) && filterChurn(record) => record
      }


  }


}


package appliednlp.classify

import nak.core.AttrVal

object CensusFeatures {

  def main(args: Array[String]) {
    val inputFile = args(0)

    io.Source.fromFile(inputFile).getLines.foreach { line =>
      val Array(age, workClass, fnlwgt, education, educationNum, maritalStatus, occupation, relationship, race, sex, capitalGain, capitalLoss, hoursPerWeek, nativeCountry, income) = line.split("[, \\.]+")
      val features = CensusFeatureExtractor(age, workClass, fnlwgt, education, educationNum, maritalStatus, occupation, relationship, race, sex, capitalGain, capitalLoss, hoursPerWeek, nativeCountry)
      println(features.map(_.toString).mkString(",") + "," + income)
    }
  }

}

object CensusFeatureExtractor {

  def apply(
    age: String, workClass: String, fnlwgt: String, education: String, educationNum: String, maritalStatus: String,
    occupation: String, relationship: String, race: String, sex: String, capitalGain: String, capitalLoss: String,
    hoursPerWeek: String, nativeCountry: String): Iterable[AttrVal] = {
    List(
      AttrVal("age", age),
      AttrVal("ageGroup", (age.toInt / 5).toString),
      AttrVal("workClass", workClass),
      AttrVal("fnlwgt", fnlwgt),
      AttrVal("fnlwgtGroup", (fnlwgt.toInt / 10000).toString),
      AttrVal("education", education),
      AttrVal("educationNum", educationNum),
      AttrVal("maritalStatus", maritalStatus),
      AttrVal("occupation", occupation),
      AttrVal("relationship", relationship),
      AttrVal("race", race),
      AttrVal("sex", sex),
      AttrVal("race+sex", race+sex),
      AttrVal("capitalGain", capitalGain),
      AttrVal("capitalGainGroup", (capitalGain.toInt / 50).toString),
      AttrVal("capitalLoss", capitalLoss),
      AttrVal("capitalLossGroup", (capitalLoss.toInt / 50).toString),
      AttrVal("capitalDiff", (capitalGain.toInt - capitalLoss.toInt).toString),
      AttrVal("hoursPerWeek", hoursPerWeek),
      AttrVal("hoursPerWeekGroup", (hoursPerWeek.toInt / 5).toString),
      AttrVal("workValue", (hoursPerWeek.toInt * educationNum.toInt).toString),
      AttrVal("nativeCountry", nativeCountry))
  }

}


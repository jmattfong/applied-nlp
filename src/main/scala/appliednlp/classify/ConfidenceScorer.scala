package appliednlp.classify

/**
 * An application that takes a gold labeled file and a file containing
 * predictions, and then computes the accuracy for the top-third most
 * confident instances and the accuracy for the bottom-third (least 
 * confident instances).
 */
object ConfidenceScorer {


  def main(args: Array[String]) {
    val delimRegex = """[\s,]+""".r
    val descs = List("Low", "Mid", "High")
    val goldLabeledFile = io.Source.fromFile(args(0)).getLines.toList
    val predictionsFile = io.Source.fromFile(args(1)).getLines.toList
    
    val sortedList = (for((l, i) <- goldLabeledFile
                                    .zip(predictionsFile)
                                    .sortBy(_ match { case (g, p) => delimRegex.split(p)(1).toDouble})
                                    .zipWithIndex)
                            yield
                            (l, Math.ceil( (i+1) * 3.0 / goldLabeledFile.length ).toInt))
                     .groupBy(_._2)
                     .toList
                     .sortBy(-_._1)
    for((i, l) <- sortedList) { 
        val count = l.count {_ match { case ((g, p),_) =>
            val truth = delimRegex.split(g).toList.last
            val pred = delimRegex.split(p)(0)
            truth == pred
        }}
        println(descs(i-1) + " confidence accuracy: " + count/l.length.toDouble)
    }
  }

}

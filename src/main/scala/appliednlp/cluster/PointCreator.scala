package appliednlp.cluster

import nak.cluster._
import nak.util.CollectionUtil._
import chalk.util.SimpleTokenizer

import org.apache.log4j.Logger
import org.apache.log4j.Level

import scala.io.Source
import scala.collection.mutable.MutableList

/**
 *  Read data and produce data points and their features.
 *
 *  @param filename the name of the file containing the data
 *  @return a triple, the first element of which is a sequence of id's
 *     (unique for each row / data point), the second element is the sequence
 *     of (known) cluster labels, and the third of which is the sequence of
 *     Points to be clustered.
 */
trait PointCreator extends (String => Iterator[(String,String,Point)])

/**
 * Read data in the standard format for use with k-means.
 */
object DirectCreator extends PointCreator {

 def apply(filename: String) = {
    Source.fromFile(filename).getLines.toList
    .map(line => {
        val parts = line.split("\\s+")
        (parts(0), parts(1), Point(Array(parts(2).toDouble, parts(3).toDouble)))
    }).toIterator
 }

}


/**
 * A standalone object with a main method for converting the achieve.dat rows
 * into a format suitable for input to RunKmeans.
 */
object SchoolsCreator extends PointCreator {

  def apply(filename: String) = {
    val result = new MutableList[(String, String, Point)]()
    Source.fromFile(filename).getLines.toList.foreach(line => {
        val parts = line.split("\\s+")
        val len = parts.length
        val name = parts.slice(0, len-4).mkString("_")
        val fourthPoint = Point(parts.slice(len-4, len-2).map(_.toDouble))
        val sixthPoint = Point(parts.slice(len-2, len).map(_.toDouble))
        result += ((name+"_4th", "4", fourthPoint))
        result += ((name+"_6th", "6", sixthPoint))
    })
    result.toIterator
  }

}

/**
 * A standalone object with a main method for converting the birth.dat rows
 * into a format suitable for input to RunKmeans.
 */
object CountriesCreator extends PointCreator {

  def apply(filename: String) = {
    Source.fromFile(filename).getLines.toList
    .map(line => {
        val parts = line.split("\\s+")
        val len = parts.length
        (parts.slice(0, len-2).mkString("_"), "1", Point(parts.slice(len-2, len).map(_.toDouble)))
    }).toIterator
  }

}

/**
 * A class that converts the raw Federalist
 * papers into rows with a format suitable for input to Cluster. As part of
 * this, it must also perform feature extraction, converting the texts into
 * sets of values for each feature (such as a word count or relative
 * frequency).
 */
class FederalistCreator(simple: Boolean = false) extends PointCreator {

  def apply(filename: String) = {
    val extractedArticles = FederalistArticleExtractor(filename)
    val ids = extractedArticles.flatMap(_.get("id"))
    val authors = extractedArticles.flatMap(_.get("author")).map(_.replaceAll("\\s+", "_"))
    val texts = extractedArticles.flatMap(_.get("text"))
    val extracted = if(simple) extractSimple(texts) else extractFull(texts)
    (ids, authors, extracted).zipped.toIterator
  }

  /**
   * Given the text of an article, compute the frequency of "the", "people"
   * and "which" and return a Point per article that has the frequency of
   * "the" as the value of the first dimension, the frequency of "people"
   * for the second, and the frequency of "which" for the third.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractSimple(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    texts.map(text => {
        val words = SimpleTokenizer(text.toLowerCase())
        val wordCounts = words.groupBy(x=>x).mapValues(x=>x.length)
        Point(Array(wordCounts.getOrElse("the", 0),
                    wordCounts.getOrElse("people", 0),
                    wordCounts.getOrElse("which", 0)))
    })
  }

  /**
   * Given the text of an article, extract features as best you can to try to
   * get good alignment of the produced clusters with the known authors.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractFull(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    val totalText = SimpleTokenizer(texts.mkString(" ").toLowerCase())
    val totalNumWords = totalText.length.toDouble
    val totalWordCounts = totalText.groupBy(x=>x).mapValues(x=>x.length)
    println(totalWordCounts.values.toList.sorted)
    texts.map(text => {
        val words = SimpleTokenizer(text.toLowerCase())
        val numWords = words.length.toDouble
        val wordCounts = words.filter(totalWordCounts.getOrElse(_,0)>5000)
            .groupBy(x=>x).mapValues(x=>x.length/numWords)
        val avgWordLen = words.map(_.length).sum / numWords
        Point(wordCounts.values.toArray)
    })
  }

}

object FederalistArticleExtractor {
  /**
   * A method that takes the raw Federalist papers input and extracts each
   * article into a structured format.
   *
   * @param filename The filename containing the Federalist papers.
   * @return A sequence of Maps (one per article) from attributes (like
   *         "title", "id", and "text") to their values for each article.
   */
  def apply(filename: String): IndexedSeq[Map[String, String]] = {

    // Regex to identify the text portion of a document.
    val JustTextRE = (
      """(?s)\*\*\* START OF THIS PROJECT GUTENBERG.+""" +
      """\*\*\*(.+)\*\*\* END OF THIS PROJECT GUTENBERG""").r

    // Regex to capture different parts of each article.
    val ArticleRE = (
      """(?s)(\d+)\n+""" + // The article number.
      """(.+?)\n+""" + // The title (note non-greedy match).
      """((?:(?:For|From)[^\n]+?)?)\s+""" + // The publication venue (optional).
      """((?:(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday).+\d\d\d\d\.)?)\n+""" + // The date (optional).
      """((?:MAD|HAM|JAY).+?)\n+""" + // The author(s).
      """(To the [^\n]+)""" + // The addressee.
      """(.+)""" // The text.
      ).r

    val book = io.Source.fromFile(filename).mkString
    val text = JustTextRE.findAllIn(book).matchData.next.group(1)
    val rawArticles = text.split("FEDERALIST.? No. ")

    // Use the regular expression to parse the articles.
    val allArticles = rawArticles.flatMap {
      case ArticleRE(id, title, venue, date, author, addressee, text) =>
        Some(Map("id" -> id.trim,
          "title" -> title.replaceAll("\\n+", " ").trim,
          "venue" -> venue.replaceAll("\\n+", " ").trim,
          "date" -> date.replaceAll("\\n+", " ").trim,
          "author" -> author.replaceAll("\\n+", " ").trim,
          "addressee" -> addressee.trim,
          "text" -> text.trim))

      case _ => None
    }.toIndexedSeq

    // Get rid of article 71, which is a duplicate, and return the rest.
    allArticles.take(70) ++ allArticles.slice(71, allArticles.length)
  }

}

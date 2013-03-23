package appliednlp.mallet

import cc.mallet.util._
import cc.mallet.types._
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.topics._

import java.io._
import java.util.{Formatter, Locale}
import java.lang.StringBuilder


object MalletModel {
    def main(args: Array[String]) {
    
        // Begin by importing documents from text to feature sequences
        val pipeList = Array(
        
        // Pipes: lowercase, tokenize, remove stopwords, map to features
        new CharSequenceLowercase(),
        new CharSequence2TokenSequence("""\p{L}[\p{L}\p{P}]+\p{L}""".r.pattern),
        new TokenSequenceRemoveStopwords(new File("stoplists/en.txt"), "UTF-8", false, false, false),
        new TokenSequence2FeatureSequence()
        )
        
        val instances = new InstanceList (new SerialPipes(pipeList))
        
        val fileReader = new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")
        instances.addThruPipe(new CsvIterator (fileReader, """^(\S*)[\s,]*(\S*)[\s,]*(.*)$""".r.pattern, 3, 2, 1)) // data, label, name fields
        
        // Create a model with 100 topics, alpha_t = 0.01, beta_w = 0.01
        //  Note that the first parameter is passed as the sum over topics, while
        //  the second is the parameter for a single dimension of the Dirichlet prior.
        val numTopics = 100
        val model = new ParallelTopicModel(numTopics, 1.0, 0.01)
        
        model.addInstances(instances)

        // Use two parallel samplers, which each look at one half the corpus and combine
        //  statistics after every iteration.
        model.setNumThreads(2)

        // Run the model for 50 iterations and stop (this is for testing only, 
        //  for real applications, use 1000 to 2000 iterations)
        model.setNumIterations(50)
        model.estimate()
        
        // Show the words and topics in the first instance

        // The data alphabet maps word IDs to strings
        val dataAlphabet : Alphabet = instances.getDataAlphabet()
        
        val tokens = model.getData().get(0).instance.getData().asInstanceOf[FeatureSequence]
        val topics : LabelSequence = model.getData().get(0).topicSequence
        
        var out = new Formatter(new StringBuilder(), Locale.US)
        
        for (position <- 0 to (tokens.getLength() - 1)) {
            out.format("%s-%d ", dataAlphabet.lookupObject(tokens.getIndexAtPosition(position)), topics.getIndexAtPosition(position).asInstanceOf[Integer])
        }
        println(out)
        
        // Estimate the topic distribution of the first instance, 
        //  given the current Gibbs state.
        val topicDistribution = model.getTopicProbabilities(0)

        // Get an array of sorted sets of word ID/count pairs
        val topicSortedWords = model.getSortedWords()
        
        // Show top 5 words in topics with proportions for the first document
        for (topic <- 0 to (numTopics - 1)) {
            val iterator = topicSortedWords.get(topic).iterator()
            
            out = new Formatter(new StringBuilder(), Locale.US)
            out.format("%d\t%.3f\t", topic, topicDistribution(topic))
            var rank = 0
            /*
            while (iterator.hasNext() && rank < 5) {
                val idCountPair = iterator.next();
                out.format("%s (%.0f) ", dataAlphabet.lookupObject(idCountPair.getID()), idCountPair.getWeight());
                rank += 1
            }
            */
            println(out);
        }

    }
}
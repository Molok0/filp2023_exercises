package exercises02

import scala.util.matching.Regex

object Counter {
  val regexCountWords: Regex        = "[\\s.,!?:()\\n\\t\\r]".r;
  val regexCountEnglishWords: Regex = "[\\s.,!?:()\\n\\t\\r[а-яА-ЯёЁ]]".r;
  val regexCountNumbers: Regex      = "[\\s!?:\\n\\t\\r]".r;
  val regexForNumber: Regex         = "(\\d*)([.,]?)(\\d*)".r

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    regexCountWords.split(text.toLowerCase()).filter(_.nonEmpty).groupMapReduce(identity)(_ => 1)(_ + _)
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    regexCountEnglishWords.split(text.toLowerCase()).filter(_.nonEmpty).groupMapReduce(identity)(_ => 1)(_ + _)
  }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    regexCountNumbers
      .split(text.toLowerCase())
      .filter(_.nonEmpty)
      .filter(_.matches(regexForNumber.regex))
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }
}

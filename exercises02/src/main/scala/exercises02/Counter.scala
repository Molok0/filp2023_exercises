package exercises02

import scala.util.matching.Regex

object Counter {
  val regexCountWords: Regex        = "[\\s.,!?:()\\n\\t\\r]".r;
  val regexCountEnglishWords: Regex = "[\\s.,!?:()\\n\\t\\r[а-яА-ЯёЁ]]".r;
  val regexCountNumbers: Regex      = "[\\s!?:\\n\\t\\r]".r;

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    text.toLowerCase.split(regexCountWords.regex).filter(a => a != "").groupMapReduce(identity)(_ => 1)(_ + _)
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    text.toLowerCase
      .split(regexCountEnglishWords.regex)
      .filter(_.nonEmpty)
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    text.toLowerCase
      .split(regexCountNumbers.regex)
      .filter(_.nonEmpty)
      .filter(_.matches("(\\d*)([.,]?)(\\d*)"))
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }
}

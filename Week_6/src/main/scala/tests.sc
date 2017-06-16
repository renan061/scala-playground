import forcomp.Anagrams

object Testing {
  def occurrences(str: String) =
    str.toLowerCase
      .groupBy(c => c).toList
      .map { case (c, s) => (c, s.length) }

  "lovee"
  occurrences("lovee")

  val concat = List("abcd", "e").reduceRight(_.concat(_))

  occurrences(concat)

  val listWords = List("I", "lovee", "nugget", "gugent", "evelo")

  listWords
    .map(s => (occurrences(s), s.toLowerCase))
    .groupBy(_._1)
    .map(p => (p._1, p._2.map(_._2)))

  val listWords2 = List("I", "lovee", "nugget")

  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    occurrences.foldRight(List[Occurrences](Nil)) {
      case ((char, counter), acc) => acc ++ (
        for {
          a <- acc
          i <- 1 to counter
        } yield (char, i) :: a
      )
    }
  }

  val abara = occurrences("abara")
  combinations(aba)

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    x map { case (c, n) =>
      (c, n - y.find(c == _._1).getOrElse((c, 0))._2)
    } filter (_._2 > 0)
  }

  val aba = occurrences("aba")

  aba map { case (char, counter) =>
    (char, counter - aba.find(char == _._1).getOrElse((char, 0))._2)
  } filter (_._2 > 0)

  val c = 'a'
  aba.find(c == _._1).getOrElse((c, 0))._2

  subtract(abara, aba)

  /* -------------------------------------------------------- */

//  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
//    val sentOcc = Anagrams.sentenceOccurrences(sentence)
//
//    def words(occ: Occurrences, combs: List[Occurrences]): List[Word] =
//      combs match {
//        case Nil => Nil
//        case x :: xs => words(occ.filterNot(_ == x))
//      }
//    }
//
//
//    val words = for {
//      comb <- combinations(sentOcc)
//    } yield Anagrams.dictionaryByOccurrences(subtract(sentOcc, comb))
//
//    Nil
//  }

  val sentence = List("I", "love", "you")
//  val sentOcc = Anagrams.sentenceOccurrences(sentence)
//  val combs = combinations(sentOcc)

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def aux(occurrences: Occurrences): List[Sentence] =
      occurrences match {
        case Nil => Nil
        case list => for {
          combs<- combinations(list)
          word <- Anagrams.dictionaryByOccurrences getOrElse (combs, Nil)
          sentence <- aux(subtract(list, Anagrams.wordOccurrences(word)))
          if !combs.isEmpty
        } yield word :: sentence
      }

    aux(Anagrams.sentenceOccurrences(sentence))
  }

  sentenceAnagrams(sentence)

//  val words = for {
//    comb <- combs
//    x <- Anagrams
//      .dictionaryByOccurrences
//      .getOrElse(subtract(sentOcc, comb), Nil)
//    if x != Nil
//  } yield x
}
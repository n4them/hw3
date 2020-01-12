package hw3

import scala.annotation.tailrec


object Main {

    class Pow(v: Double) {
        def ^(implicit y: Double): Double = math.pow(v, y)
    }

    implicit def doubleToPow(x: Double): Pow = new Pow(x)


    def standardDeviation(vector: List[Double]): Double = {
        require(vector.nonEmpty, "Vector is empty")
        val n = vector.length
        val Xn = vector.sum / n
        val variation = vector.foldLeft(0.0)((acc, x) => ((x - Xn) ^ 2) + acc) / n // should be n - 1
        variation ^ 0.5
    }


    def seqToString(letters: Seq[Char]): String = {
        val result: StringBuilder = new StringBuilder()
        for (c <- letters) {
            result += c
        }
        result.toString()
    }

    def letterFrequencyRanking(corpus: String): String = {

        case class Alphabet(alphabet: Map[Char, Int] = Map.empty) {
            def add(char: Char): Alphabet = {
                alphabet.get(char) match {
                    case Some(value) => Alphabet(alphabet.updated(char, value + 1))
                    case None => Alphabet(alphabet + (char -> 1))
                }
            }
        }

        @tailrec
        def parserCorups(corpus: List[Char], letters: Alphabet): Alphabet = {
            if (corpus.nonEmpty) {
                if (corpus.head >= 'a' && corpus.head <= 'z') {
                    parserCorups(corpus.tail, letters.add(corpus.head))
                } else
                    parserCorups(corpus.tail, letters)
            } else
                letters
        }

        val lettersFrequency: Alphabet = parserCorups(corpus.toLowerCase.toList, Alphabet())
        val orderedLetters = lettersFrequency.alphabet
            .toSeq
            .sortWith((a, b) => (a._2 > b._2) || (a._2 == b._2 && a._1 < b._1))
            .map(a => a._1)
        seqToString(orderedLetters)
    }


    def gray(bits: Int): List[String] = ???

    def romanji(katakana: String): String = {
        val before = "[ュャョー]".r
        val after = "[ッ]".r
        val skipSymbols = "[ ,!\n?]".r

        def loop(i: Int, list: List[Char]): List[Char] = {
            def parser(i: Int, list: List[Char]): List[Char] = katakana.charAt(i) match {
                case char: Char if before.matches(char.toString) &&
                    ((i == 0) || skipSymbols.matches(list.head.toString)) =>
                    throw new IllegalArgumentException(s"No symbols before ${katakana.charAt(i)}; i: $i")

                case char: Char if after.matches(char.toString) &&
                    (i == katakana.length - 1 || skipSymbols.matches(katakana.charAt(i + 1).toString)) =>
                    throw new IllegalArgumentException(s"No symbols after ${katakana.charAt(i)}; i: $i")

                case char: Char if char == 'ュ' || char == 'ャ' || char == 'ョ' => list.head match {
                    case 'i' => {
                        char match {
                            case 'ュ' => 'u'
                            case 'ャ' => 'a'
                            case _ => 'o'
                        }
                    } :: 'y' :: list.tail
                    case _ => throw new IllegalArgumentException(s"No i before $char; i: $i")
                }
                case 'ッ' => //double next not n
                    parser(i + 1, list).tail.head match {
                        case 'n' => throw new IllegalArgumentException(s"ッ before n; i: $i")
                        case char: Char => /*next.head ::*/ char :: list
                    }
                /*case 'ン' => //double next n
                    parser(i + 1, list).tail.head match {
                        case 'n' => /*next.head ::*/ 'n' :: list
                        case _ => throw new IllegalArgumentException(s"ン before not n; i: $i")
                    }*/
                case 'ー' => Katakana.findLongVowel(list.head) :: list.tail //vowel
                case char: Char if skipSymbols.matches(char.toString) => char :: list
                case char: Char => Katakana.findSymbol(char) ::: list
            }

            if (i >= katakana.length)
                list
            else {
                println(list.reverse)
                loop(i + 1, parser(i, list))
            }
        }

        seqToString(loop(0, List.empty[Char]).reverse)
    }

    def ア(): Unit = {

    }
}

object Katakana {
    val symbols = Map(
        'ア' -> List('a'), 'イ' -> List('i'), 'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
        'ン' -> List('n'),
        'カ' -> List('a', 'k'), 'キ' -> List('i', 'k'), 'ク' -> List('u', 'k'), 'ケ' -> List('e', 'k'), 'コ' -> List('o', 'k'),
        'ガ' -> List('a', 'g'), 'ギ' -> List('i', 'g'), 'グ' -> List('u', 'g'), 'ゲ' -> List('e', 'g'), 'ゴ' -> List('o', 'g'),
        'サ' -> List('a', 's'), 'シ' -> List('i', 's'), 'ス' -> List('u', 's'), 'セ' -> List('e', 's'), 'ソ' -> List('o', 's'),
        'ザ' -> List('a', 'z'), 'ジ' -> List('i', 'z'), 'ズ' -> List('u', 'z'), 'ゼ' -> List('e', 'z'), 'ゾ' -> List('o', 'z'),
        'タ' -> List('a', 't'), 'チ' -> List('i', 't'), 'ツ' -> List('u', 't'), 'テ' -> List('e', 't'), 'ト' -> List('o', 't'),
        'ダ' -> List('a', 'd'), 'ヂ' -> List('i', 'd'), 'ヅ' -> List('u', 'd'), 'デ' -> List('e', 'd'), 'ド' -> List('o', 'd'),
        'ナ' -> List('a', 'n'), 'ニ' -> List('i', 'n'), 'ヌ' -> List('u', 'n'), 'ネ' -> List('e', 'n'), 'ノ' -> List('o', 'n'),
        'ハ' -> List('a', 'h'), 'ヒ' -> List('i', 'h'), 'フ' -> List('u', 'h'), 'ヘ' -> List('e', 'h'), 'ホ' -> List('o', 'h'),
        'バ' -> List('a', 'b'), 'ビ' -> List('i', 'b'), 'ブ' -> List('u', 'b'), 'ベ' -> List('e', 'b'), 'ボ' -> List('o', 'b'),
        'パ' -> List('a', 'p'), 'ピ' -> List('i', 'p'), 'プ' -> List('u', 'p'), 'ペ' -> List('e', 'p'), 'ポ' -> List('o', 'p'),
        'マ' -> List('a', 'm'), 'ミ' -> List('i', 'm'), 'ム' -> List('u', 'm'), 'メ' -> List('e', 'm'), 'モ' -> List('o', 'm'),
        'ヤ' -> List('a', 'y'), 'ユ' -> List('u', 'y'), 'ヨ' -> List('o', 'y'),
        'ラ' -> List('a', 'r'), 'リ' -> List('i', 'r'), 'ル' -> List('u', 'r'), 'レ' -> List('e', 'r'), 'ロ' -> List('o', 'r'),
        'ワ' -> List('a', 'w'), 'ヰ' -> List('i', 'w'), 'ヱ' -> List('e', 'w'), 'ヲ' -> List('o', 'w'),
    )

    def findSymbol(char: Char): List[Char] = symbols.get(char) match {
        case Some(value) => value
        case None => throw new IllegalArgumentException(s"Illegal symbol:$char")
    }

    def findLongVowel(char: Char): Char = longVowels.get(char) match {
        case Some(value) => value
        case None => throw new IllegalArgumentException(s"Illegal long vowel:$char")
    }

    val longVowels = Map(
        'a' -> 'ā',
        'i' -> 'ī',
        'e' -> 'ē',
        'u' -> 'ū',
        'o' -> 'ō'
    )
}

/*
val symbols = Map(
        'ア' -> List('a'), 'イ' -> List('i'), 'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
        'ン' -> List('n'),
        'カ' -> List('k', 'a'), 'キ' -> List('k', 'i'), 'ク' -> List('k', 'u'), 'ケ' -> List('k', 'e'), 'コ' -> List('k', 'o'),
        'ガ' -> List('g', 'a'), 'ギ' -> List('g', 'i'), 'グ' -> List('g', 'u'), 'ゲ' -> List('g', 'e'), 'ゴ' -> List('g', 'o'),
        'サ' -> List('s', 'a'), 'シ' -> List('s', 'i'), 'ス' -> List('s', 'u'), 'セ' -> List('s', 'e'), 'ソ' -> List('s', 'o'),
        'ザ' -> List('z', 'a'), 'ジ' -> List('z', 'i'), 'ズ' -> List('z', 'u'), 'ゼ' -> List('z', 'e'), 'ゾ' -> List('z', 'o'),
        'タ' -> List('t', 'a'), 'チ' -> List('t', 'i'), 'ツ' -> List('t', 'u'), 'テ' -> List('t', 'e'), 'ト' -> List('t', 'o'),
        'ダ' -> List('d', 'a'), 'ヂ' -> List('d', 'i'), 'ヅ' -> List('d', 'u'), 'デ' -> List('d', 'e'), 'ド' -> List('d', 'o'),
        'ナ' -> List('n', 'a'), 'ニ' -> List('n', 'i'), 'ヌ' -> List('n', 'u'), 'ネ' -> List('n', 'e'), 'ノ' -> List('n', 'o'),
        'ハ' -> List('h', 'a'), 'ヒ' -> List('h', 'i'), 'フ' -> List('h', 'u'), 'ヘ' -> List('h', 'e'), 'ホ' -> List('h', 'o'),
        'バ' -> List('b', 'a'), 'ビ' -> List('b', 'i'), 'ブ' -> List('b', 'u'), 'ベ' -> List('b', 'e'), 'ボ' -> List('b', 'o'),
        'パ' -> List('p', 'a'), 'ピ' -> List('p', 'i'), 'プ' -> List('p', 'u'), 'ペ' -> List('p', 'e'), 'ポ' -> List('p', 'o'),
        'マ' -> List('m', 'a'), 'ミ' -> List('m', 'i'), 'ム' -> List('m', 'u'), 'メ' -> List('m', 'e'), 'モ' -> List('m', 'o'),
        'ヤ' -> List('y', 'a'), 'ユ' -> List('y', 'u'), 'ヨ' -> List('y', 'o'),
        'ラ' -> List('r', 'a'), 'リ' -> List('r', 'i'), 'ル' -> List('r', 'u'), 'レ' -> List('r', 'e'), 'ロ' -> List('r', 'o'),
        'ワ' -> List('w', 'a'), 'ヰ' -> List('w', 'i'), 'ヱ' -> List('w', 'e'), 'ヲ' -> List('w', 'o'),
    )
 */
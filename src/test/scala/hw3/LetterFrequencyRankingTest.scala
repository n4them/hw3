package hw3

import hw3.Main.letterFrequencyRanking
import org.scalatest.{FunSuite, Matchers}

class LetterFrequencyRankingTest extends FunSuite with Matchers {
    test("Simple. Empty") {
        letterFrequencyRanking("") shouldBe ""
    }
    test("Simple") {
        letterFrequencyRanking("hello") shouldBe "leho"
    }
    test("Capital letters") {
        letterFrequencyRanking("AaaAaaAaa") shouldBe "a"
    }
    test("Capital letters. Hard") {
        letterFrequencyRanking("AaaAaaAaaBBBBbbBBBbBBBb") shouldBe "ba"
    }
    test("Punctuation") {
        letterFrequencyRanking("Sic!") shouldBe "cis"
    }
    test("Punctuation. Middle") {
        letterFrequencyRanking("...") shouldBe ""
    }
    test("Punctuation. Hard") {
        letterFrequencyRanking("Sic! hard,\n hard, hard.") shouldBe "adhrcis"
    }
    test("Numbers") {
        letterFrequencyRanking("516156154 65456 6546") shouldBe ""
    }

    test("Numbers with letters") {
        letterFrequencyRanking("5161Abc56154 asd 65456 6546BCA") shouldBe "abcds"
    }
}
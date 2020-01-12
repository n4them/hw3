package hw3

import hw3.Main.romanji
import org.scalatest.{FunSuite, Matchers}

class RomanjiTest extends FunSuite with Matchers {
    test("Toilet") {
        romanji("トイレ") shouldBe "toire"
    }
    test("Television") {
        romanji("テレビ") shouldBe "terebi"
    }
    test("Drama") {
        romanji("ドラマ") shouldBe "dorama"
    }
    test("Ice-cream") {
        romanji("アイスクリーム") shouldBe "aisukurīmu"
    }
    test("Knock") {
        romanji("ノック") shouldBe "nokku"
    }

    test("ュ test") {
        romanji("ギュク") shouldBe "gyuku"
    }
    test("ュ test. fail") {
        val thrown = the [IllegalArgumentException] thrownBy romanji("ガュク")
        thrown.getMessage shouldBe "No i before ュ; i: 1"
    }
    test("ャ test") {
        romanji("ギャク") shouldBe "gyaku"
    }
    test("ャ test. fail"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ガャク")
        thrown.getMessage shouldBe "No i before ャ; i: 1"
    }
    test("ョ test"){
        romanji("ギョク") shouldBe "gyoku"
    }
    test("ョ test. fail"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ガョク")
        thrown.getMessage shouldBe "No i before ョ; i: 1"
    }
    test("ン test"){
        romanji("ギクンニ") shouldBe "gikunni"
    }
    /*test("ン test. fail"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ギクンク")
        thrown.getMessage shouldBe "ン before not n; i: 2"
    }*/
    test("ッ test") {
        romanji("ノック") shouldBe "nokku"
    }
    test("ッ test. fail") {
        val thrown = the [IllegalArgumentException] thrownBy romanji("ノッニ")
        thrown.getMessage shouldBe "ッ before n; i: 1"
    }
    test("ー test"){
        romanji("メール") shouldBe "mēru"
    }
    test("ー test. fail"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ンール")
        thrown.getMessage shouldBe "Illegal long vowel:n"
    }
    test("Illegal symbol"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("トイレa")
        thrown.getMessage shouldBe "Illegal symbol:a"
    }

    test("skip symbols"){
        romanji("トイレ テレビ") shouldBe "toire terebi"
        romanji("トイレ テレビ!") shouldBe "toire terebi!"
        romanji("トイレ テレビ?") shouldBe "toire terebi?"
        romanji("トイレ, テレビ") shouldBe "toire, terebi"
        romanji("トイレ\nテレビ") shouldBe "toire\nterebi"
    }

    test("before symbols fail. ー"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ノ ール")
        thrown.getMessage shouldBe "No symbols before ー; i: 2"
    }

    test("before symbols fail.ー"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ール")
        thrown.getMessage shouldBe "No symbols before ー; i: 0"
    }

    test("after symbols fail.ッ"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ノッ!")
        thrown.getMessage shouldBe "No symbols after ッ; i: 1"
    }

    test("after symbols fail.ッ,"){
        val thrown = the [IllegalArgumentException] thrownBy romanji("ノッ")
        thrown.getMessage shouldBe "No symbols after ッ; i: 1"
    }




}
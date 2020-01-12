package hw3

import hw3.Main.gray
import hw3.Main.gray2
import org.scalatest.{FunSuite, Matchers}

class GrayTest extends FunSuite with Matchers {

    test("Gray example 0 bit") {
        val thrown = the[IllegalArgumentException] thrownBy gray(0)
        thrown.getMessage shouldBe "requirement failed"
    }

    test("Gray example 1 bit") {
        gray(1) shouldBe List("0", "1")
    }

    test("Gray example 2 bits") {
        gray(2) shouldBe List("00", "01", "11", "10")
    }

    test("Gray example 3 bits") {
        gray(3) shouldBe List("000", "001", "011", "010", "110", "111", "101", "100")
    }
    /*def speedTest(i: Int, f: Int => List[String]): Unit ={
        f(i)
    }

    def speedTest1(f: Int => List[String]) = speedTest(20,f)
    def speedTest2(f: Int => List[String]) = speedTest(23,f)
    test("1.1"){
        speedTest1(gray)
    }

    test("2.1"){
        speedTest1(gray2)
    }
    test("1.2"){
        speedTest2(gray)
    }

    test("2.2"){
        speedTest2(gray2)
    }
*/




}
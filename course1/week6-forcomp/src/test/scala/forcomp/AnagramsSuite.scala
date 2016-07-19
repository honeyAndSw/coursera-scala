package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: eat") {
    val occursFromDictionary: Option[Set[Word]] = dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet)
    assert(occursFromDictionary === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
        List(),
        List(('a', 1)),
        List(('a', 2)),
        List(('b', 1)),
        List(('a', 1), ('b', 1)),
        List(('a', 2), ('b', 1)),
        List(('b', 2)),
        List(('a', 1), ('b', 2)),
        List(('a', 2), ('b', 2))
    )

    println(combinations(abba))
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: abbac") {
    val abbac = List(('a', 2), ('b', 2), ('c', 1))
    val abbaccomb = List(
      List(),

      List(('a', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 1), ('c', 1)),
      List(('a', 1), ('b', 2)),
      List(('a', 1), ('b', 2), ('c', 1)),
      List(('a', 1), ('c', 1)),

      List(('a', 2)),
      List(('a', 2), ('b', 1)),
      List(('a', 2), ('b', 1), ('c', 1)),
      List(('a', 2), ('b', 2)),
      List(('a', 2), ('c', 1)),
      List(('a', 2), ('b', 2), ('c', 1)),

      List(('b', 1)),
      List(('b', 1), ('c', 1)),
      List(('b', 2)),
      List(('b', 2), ('c', 1)),

      List(('c', 1))
    )

    println(combinations(abbac))
    assert(combinations(abbac).toSet === abbaccomb.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: jimmy - my") {
    val jimmy = List(('i', 1), ('j', 1), ('m', 2), ('y', 1))
    val my = List(('m', 1), ('y', 1))
    val jim = List(('i', 1), ('j', 1), ('m', 1))
    assert(subtract(jimmy, my) === jim)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Heather") {
    val sentence = List("Heather")
    val anas = List(List("re", "the", "ha"), List("he", "hat", "re"), List("three", "ha"), List("hare", "the"), List("her", "Thea"), List("at", "he", "her"), List("her", "et", "ha"), List("re", "ah", "the"), List("he", "he", "art"), List("hear", "the"), List("et", "her", "ha"), List("he", "re", "hat"), List("hat", "re", "he"), List("heather"), List("he", "heart"), List("re", "ha", "the"), List("here", "hat"), List("he", "at", "her"), List("he", "hater"), List("ah", "three"), List("her", "heat"), List("he", "earth"), List("there", "ha"), List("ha", "re", "the"), List("re", "heath"), List("et", "ha", "her"), List("ah", "et", "her"), List("ah", "her", "et"), List("re", "the", "ah"), List("he", "he", "rat"), List("rat", "he", "he"), List("ha", "ether"), List("ha", "her", "et"), List("he", "tar", "he"), List("he", "rat", "he"), List("at", "her", "he"), List("ha", "there"), List("ether", "ha"), List("art", "he", "he"), List("ah", "re", "the"), List("tar", "he", "he"), List("hate", "her"), List("the", "hear"), List("he", "art", "he"), List("earth", "he"), List("Rhea", "the"), List("three", "ah"), List("et", "ah", "her"), List("her", "et", "ah"), List("the", "hare"), List("the", "re", "ah"), List("re", "hat", "he"), List("her", "at", "he"), List("hat", "he", "re"), List("he", "he", "tar"), List("ha", "the", "re"), List("the", "Hera"), List("ah", "the", "re"), List("et", "her", "ah"), List("ha", "three"), List("heat", "her"), List("hater", "he"), List("the", "ha", "re"), List("he", "her", "at"), List("heart", "he"), List("there", "ah"), List("ah", "there"), List("ah", "ether"), List("Hera", "the"), List("her", "he", "at"), List("hat", "here"), List("ha", "et", "her"), List("the", "ah", "re"), List("her", "ah", "et"), List("ether", "ah"), List("the", "Rhea"), List("her", "hate"), List("re", "he", "hat"), List("heath", "re"), List("Thea", "her"), List("her", "ha", "et"), List("the", "re", "ha"))
    val anagrams: List[Sentence] = sentenceAnagrams(sentence)
    println(anagrams)
    assert(anagrams.toSet === anas.toSet)
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    val anagrams: List[Sentence] = sentenceAnagrams(sentence)
    println(anagrams)
    assert(anagrams.toSet === anas.toSet)
  }

}

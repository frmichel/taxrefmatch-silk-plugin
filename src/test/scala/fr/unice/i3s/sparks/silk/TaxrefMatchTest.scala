package fr.unice.i3s.sparks.silk

import org.junit.Assert._
import org.junit.Test
import java.util.regex.Pattern

class TaxrefMatchTest {

  def normalizeSpaces(name: String): String = {
    name.replaceAll("""(\s)+""", " ").replace('\u00a0', ' ').trim
  }

  @Test def TestNormalizeSpaces() {
    println("------------------ TestReduceSpaces ------------------")

    var sb = new StringBuffer("ab   cd 	ef")
    TaxrefMatch.normalizeSpaces(sb)
    assertEquals("ab cd ef", sb.toString())

    sb = new StringBuffer("ab 		cd 		 ef")
    TaxrefMatch.normalizeSpaces(sb)
    assertEquals("ab cd ef", sb.toString)

    sb = new StringBuffer("  ab		cd ef		")
    TaxrefMatch.normalizeSpaces(sb)
    assertEquals("ab cd ef", sb.toString)
  }

  @Test def TestExtractBracketTerm() {
    println("------------------ TestExtractBracketTerm ------------------")
    assertEquals("1st 3rd 4th", TaxrefMatch.extractBracketTerm(new StringBuffer("1st (2nd) 3rd 4th")))
    assertEquals("1st   (2nd)", TaxrefMatch.extractBracketTerm(new StringBuffer("1st   (2nd)"))) // no change
    assertEquals("(2nd) 3rd  ", TaxrefMatch.extractBracketTerm(new StringBuffer("(2nd) 3rd  "))) // no change
    assertEquals("Barbatia bistrigata (Dunker, 1866)", TaxrefMatch.extractBracketTerm(new StringBuffer("Barbatia (Mesocibota) bistrigata (Dunker, 1866)")))

    assertEquals("1st 3rd 4th", TaxrefMatch.extractBracketTerm(new StringBuffer("1st [2nd] 3rd 4th")))
    assertEquals("1st [2nd]", TaxrefMatch.extractBracketTerm(new StringBuffer("1st [2nd]"))) // no change
    assertEquals("[2nd] 3rd", TaxrefMatch.extractBracketTerm(new StringBuffer(normalizeSpaces("[2nd] 3rd ")))) // no change
    assertEquals("Aphis ficus Theobald, [1918]", TaxrefMatch.extractBracketTerm(new StringBuffer("Aphis [?] ficus Theobald, [1918]")))

    assertEquals("1st (3rd) 4th", TaxrefMatch.extractBracketTerm(new StringBuffer("1st [2nd] (3rd) 4th")))
    assertEquals("1st [3rd] 4th", TaxrefMatch.extractBracketTerm(new StringBuffer("1st (2nd) [3rd] 4th")))
  }

  @Test def TestFilterCharacters() {
    println("------------------ TestFilterCharacters ------------------")
    assertEquals("AB12CDEF", TaxrefMatch.filterCharacters("AB12CDEF"))
    assertEquals("ABCDEF", TaxrefMatch.filterCharacters("AB12CDEF", false))
    assertEquals("AB 1.2CDEF", TaxrefMatch.filterCharacters("AB 1.2	&	CDEF@#{[\n"))
  }

  @Test def TestNormalizeAuthor() {
    println("------------------ TestNormalizeAuthor ------------------")

    var sb = new StringBuffer("BUNIAS L.")
    TaxrefMatch.normalizeAuthor(sb)
    assertEquals("BUNIAS LINNEAUS", sb.toString())

    sb = new StringBuffer("ACETABULARIA ACETABULUM (L.) P.C.SILVA")
    TaxrefMatch.normalizeAuthor(sb)
    assertEquals("ACETABULARIA ACETABULUM  LINNEAUS P.C.SILVA", sb.toString())

    sb = new StringBuffer("TOTO TURLU ET AL")
    TaxrefMatch.normalizeAuthor(sb)
    assertEquals("TOTO TURLU ET AL", sb.toString())

    sb = new StringBuffer("TOTO TURLU ET TUTU")
    TaxrefMatch.normalizeAuthor(sb)
    assertEquals("TOTO TURLU & TUTU", sb.toString())
  }

  @Test def TestNormalize() {
    println("------------------ TestNormalize ------------------")
    assertEquals("AB12CDEF", TaxrefMatch.normalize("ab12cdef"))
    assertEquals("BARBATIA BISTRIGATA DUNKER 1866", TaxrefMatch.normalize("Barbatia [2nd term]\t \n \f bistrigata  - (Dunker, 1866) "))
    assertEquals("BRASSICA REPANDA REPANDA AA BB 1866", TaxrefMatch.normalize("Brassica repanda subsp. repanda <i>(Aa &amp; Bb , 1866)</i>"))
    assertEquals("BRASSICA AA BB CC DD", TaxrefMatch.normalize("""Brassica "Aa"×Bb «cc» ˝dd ˝ … """))
    assertEquals("OPAS", TaxrefMatch.normalize("Opas"))
  }
}

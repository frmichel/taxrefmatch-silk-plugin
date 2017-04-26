package fr.unice.i3s.sparks.silk

import org.junit.Assert._
import org.junit.Test
import java.util.regex.Pattern

class TaxrefMatchTest {

  @Test def TestNormalizeSpaces() {
    println("------------------ TestReduceSpaces ------------------")
    assertEquals("ab cd ef", TaxrefMatch.normalizeSpaces("ab   cd 	ef"))
    assertEquals("ab cd ef", TaxrefMatch.normalizeSpaces("ab 		cd 		 ef"))
    assertEquals(" ab cd ef ", TaxrefMatch.normalizeSpaces("  ab		cd ef		"))
  }

  @Test def TestExtractBracketTerm() {
    println("------------------ TestExtractBracketTerm ------------------")
    assertEquals("1st 3rd 4th", TaxrefMatch.extractBracketTerm("1st   (2nd) 	3rd 4th"))
    assertEquals("1st   (2nd)", TaxrefMatch.extractBracketTerm("1st   (2nd)")) // no change
    assertEquals("(2nd) 3rd  ", TaxrefMatch.extractBracketTerm("(2nd) 3rd  ")) // no change
    assertEquals("Barbatia bistrigata (Dunker, 1866)", TaxrefMatch.extractBracketTerm("Barbatia (Mesocibota)   bistrigata (Dunker, 1866)"))

    assertEquals("1st 3rd 4th", TaxrefMatch.extractBracketTerm("1st [2nd] 	3rd 4th"))
    assertEquals("1st   [2nd]", TaxrefMatch.extractBracketTerm("1st   [2nd]")) // no change
    assertEquals("[2nd] 3rd  ", TaxrefMatch.extractBracketTerm("[2nd] 3rd  ")) // no change
    assertEquals("Aphis ficus Theobald, [1918]", TaxrefMatch.extractBracketTerm("Aphis [?] ficus  Theobald, [1918]"))

    assertEquals("1st (3rd) 4th", TaxrefMatch.extractBracketTerm("1st   [2nd] 	(3rd) 4th"))
    assertEquals("1st [3rd] 4th", TaxrefMatch.extractBracketTerm("1st   (2nd) 	[3rd] 4th"))
  }

  @Test def TestFilterCharacters() {
    println("------------------ TestFilterCharacters ------------------")
    assertEquals("ab12cdef", TaxrefMatch.filterCharacters("ab12cdef"))
    assertEquals("abcdef", TaxrefMatch.filterCharacters("ab12cdef", false))
    assertEquals("AB 1.2\t\tcdef\n", TaxrefMatch.filterCharacters("AB 1.2	&	cdef@#{[\n"))
  }

  @Test def TestNormalize() {
    println("------------------ TestNormalize ------------------")
    assertEquals("AB12CDEF", TaxrefMatch.normalize("ab12cdef"))
    assertEquals("BARBATIA BISTRIGATA DUNKER 1866", TaxrefMatch.normalize("Barbatia [?]	 bistrigata  - (Dunker, 1866) "))
    assertEquals("BRASSICA REPANDA REPANDA AA BB 1866", TaxrefMatch.normalize("Brassica repanda subsp. repanda <i>(Aa &amp; Bb , 1866)</i>"))
    assertEquals("BRASSICA AA BB CC DD", TaxrefMatch.normalize("""Brassica "Aa"×Bb «cc» ˝dd ˝ … """))
  }
}

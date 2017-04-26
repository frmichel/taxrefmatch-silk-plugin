package fr.unice.i3s.sparks.silk

import java.util.logging.Logger
import java.util.regex.Pattern

/**
 * @author Franck Michel, I3S laboratory (this Scala port)
 * @author Olivier Gargominy, INPN (TAXREF-MATCH)
 * @author Tony Rees (orignial version)
 */
object TaxrefMatch {

  val RXSecondTermInRoundBrackets = Pattern.compile("""^([^\(\)\s]+) (\([^\s]+\)) (.+)""")
  val RXSecondTermInSquareBrackets = Pattern.compile("""^([^\[\]\s]+) (\[[^\s]+\]) (.+)""")

  val logger = Logger.getLogger(getClass.getName)

  /**
   * Replace any sequence of whitespace characters with a single space.
   * This handles simple white space, tab, CR, LF and non-breaking space
   */
  def normalizeSpaces(name: String): String = {
    name.replaceAll("""(\s)+""", " ").replace('\u00a0', ' ')
  }

  /**
   * Remove second term within brackets:
   * <ul>
   * <li>if second term (only) is within round brackets, presume it is a subgenus or a comment and remove it.
   *     examples: Barbatia (Mesocibota) bistrigata (Dunker, 1866) => Barbatia bistrigata (Dunker, 1866)</li>
   * <li>if second term (only) is within square brackets, presume it is a comment and remove it.
   *     example: Aphis [?] ficus Theobald, [1918] => Aphis ficus Theobald, [1918]<br>
   *     (this will not suit genus + author alone, where first part of authorname is in brackets,
   *     however this is very rare and in any case we are not supporting genus+authority in this version)</li>
   * </ul>
   */
  def extractBracketTerm(name: String): String = {
    val matcherRnd = RXSecondTermInRoundBrackets.matcher(normalizeSpaces(name))
    if (matcherRnd.matches)
      return matcherRnd.group(1) + " " + matcherRnd.group(3)

    val matcherSq = RXSecondTermInSquareBrackets.matcher(normalizeSpaces(name))
    if (matcherSq.matches)
      return matcherSq.group(1) + " " + matcherSq.group(3)

    name
  }

  /**
   * Remove all but alphabet letters, spaces, full stops
   * (used on scientific name components only, not authorities) and numbers if with_date is true
   *
   * @param name string in uppercase or lowercase
   * @param keepNum whether the keep figures (default true)
   */
  def filterCharacters(name: String, keepNum: Boolean = true): String = {
    name.map(x => x match {
      case c if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) => c
      case c if (('0' <= c && c <= '9') && keepNum) => c
      case c if (c == '.') => c
      case c if (c == ' ' || c == '\t' || c == '\r' || c == '\n') => c
      case _ => ""
    }).toList.mkString
  }

  /**
   * Produce a normalized version of an input string (scientific name components)
   * <ol>
   * <li>Remove known text elements e.g. 'aff.', 'cf.', 'subsp.', subgenus if enclosed in roud brackets </li>
   * <li>Remove accented and non A-Z characters other than full stops (in scientific name portions)</li>
   * <li>Return uppercase scientific name (genus + species only) plus unaltered (presumed) authority, e.g.:
   * - Anabaena cf. flos-aquae Ralfs ex Born. et Flah. => ANABAENA FLOSAQUAE Ralfs ex Born. et Flah.<br>
   * - Abisara lemée-pauli => ABISARA LEMEEPAULI<br>
   * - Fuc/us Vesiculos2us => FUCUS VESICULOSUS<br>
   * - Buffo ignicolor Lacépède, 1788 => BUFFO IGNICOLOR Lacépède, 1788<br>
   * - Barbatia (Mesocibota) bistrigata (Dunker, 1866) => BARBATIA BISTRIGATA (Dunker, 1866)<br>
   * </li>
   * <li>This version does not handle genus+author, or genus+species+infraspecies: second "good" term is
   *     presumed to be species epithet, anything after is considered to be start of the authority</li>
   * <li>There is a separate function "normalize_auth" for normalizing authorities when required
   *     (e.g. for authority comparisons)</li>
   *  </ol>
   *
   * @param name input name presumably genus, genus+species, or genus+species+authority
   * @param withDate indicates whether the year is part of the authority (default true)
   * @return normalized version of input name
   */
  def normalize(name: String, withDate: Boolean = true): String = {
    var tmpName = name

    // Replace any sequence of white spaces by a single ' ', trim leading/trailing spaces
    tmpName = normalizeSpaces(tmpName).trim
    tmpName = tmpName.toUpperCase

    // Replace any HTML ampersands
    tmpName = tmpName.replaceAllLiterally("&AMP;", "&")

    // Replace special characters
    // See list of UTF-16 codes at http://www.fileformat.info/info/charset/UTF-16/list.htm
    tmpName = tmpName.replaceAllLiterally("\u00df", "beta") // lowercase beta: ß
    tmpName = tmpName.replaceAllLiterally("\u03b2", "beta") // lowercase beta: β
    tmpName = tmpName.replaceAllLiterally("\u0392", "beta") // uppercase beta: Β

    tmpName = tmpName.map(x => x match {
      case '\u2026' => ' ' // ellipsis (three dots in one): …
      case '\u00d7' => 'x' // multiplication sign: ×
      case '\u0027' => "" // quote: '
      case '\u0022' => "" // quotation mark: " (double quote)
      case '\u02bb' => "" // turned comma: ʻ
      case '\u02bc' => "" // apostrophe: ʼ
      case '\u02bd' => "" // reversed comma: ʽ
      case '\u02ca' => "" // acute accent: ˊ (fake apostrophe)
      case '\u02cb' => "" // grave accent: ˋ
      case '\u02dd' => "" // double acute accent: ˝
      case '\u030f' => "" // double grave accent: ̏
      case '\u0093' => "" // double turned comma: ̏“
      case '\u0094' => "" // double apostrophe: ̏”
      case '\u00ab' => "" // left double angle quotation: «
      case '\u00bb' => "" // right double angle quotation: »
      case c => c
    }).toList.mkString

    // Remove any content in angle brackets (e.g. html tags - <i>, </i>, etc.)
    tmpName = tmpName.replaceAll("""<[^<>]+>""", " ")

    // Remove second term (only) in round or square brackets
    tmpName = extractBracketTerm(tmpName)

    // Drop indicators of questionable id's, subspecies, varieties
    /* tmpName = tmpName.replaceAllLiterally(""" CF """, " ")
    tmpName = tmpName.replaceAllLiterally(""" CF. """, " ")
    tmpName = tmpName.replaceAllLiterally(""" NEAR """, " ")
    tmpName = tmpName.replaceAllLiterally(""" AFF """, " ")
    tmpName = tmpName.replaceAllLiterally(""" AFF. """, " ") */
    tmpName = tmpName.replaceAllLiterally(""" SP.""", " ")
    tmpName = tmpName.replaceAllLiterally(""" SPP.""", " ")
    tmpName = tmpName.replaceAllLiterally(""" SPP """, " ")
    tmpName = tmpName.replaceAllLiterally(""" SSP.""", " ")
    tmpName = tmpName.replaceAllLiterally(""" SUBSP.""", " ")
    tmpName = tmpName.replaceAllLiterally(""" F. """, " ")
    tmpName = tmpName.replaceAllLiterally(""" VAR. """, " ")
    tmpName = tmpName.replaceAllLiterally(""" FORM """, " ")
    tmpName = tmpName.replaceAllLiterally(""" SUVAR. """, " ")

    // Replace any accented characters
    tmpName = tmpName.map(x => x match {
      case '\u00c0' => "A" // À
      case '\u00c1' => "A" // Á
      case '\u00c2' => "A" // Â
      case '\u00c3' => "A" // Ã
      case '\u00c4' => "A" // Ä
      case '\u00c5' => "A" // Å
      case '\u00c6' => "AE" // Æ ligature

      case '\u00C7' => "C" // Ç
      case '\u010c' => "C" // Č
      case '\u0106' => "C" // Ć
      case '\u0108' => "C" // Ĉ
      case '\u010a' => "C" // Ċ

      case '\u010e' => "D" // Ď
      case '\u0110' => "D" // Đ        

      case '\u00c8' => "E" // È
      case '\u00c9' => "E" // É
      case '\u00ca' => "E" // Ê
      case '\u00cb' => "E" // Ë
      case '\u0114' => "E" // Ĕ

      case '\u00cc' => "I" // Ì
      case '\u00cd' => "I" // Í
      case '\u00ce' => "I" // Î
      case '\u00cf' => "I" // Ï

      case '\u00d2' => "O" // Ò
      case '\u00d3' => "O" // Ó
      case '\u00d4' => "O" // Ô
      case '\u00d5' => "O" // Õ
      case '\u00d6' => "O" // Ö
      case '\u00d8' => "O" // Ø
      case '\u0152' => "OE" // Œ ligature

      case '\u00d9' => "U" // Ù
      case '\u00da' => "U" // Ú
      case '\u00db' => "U" // Û
      case '\u00dc' => "U" // Ü
      case '\u016e' => "U" // Ů

      case '\u00d1' => "N" // Ñ
      case '\u0160' => "S" // Š
      case '\u00dd' => "Y" // Ý
      case '\u0178' => "Y" // Ÿ
      case '\u017d' => "Z" // Ž

      case c => c
    }).toList.mkString

    // Drop any chars other than A-Z, space, and full stop
    tmpName = filterCharacters(tmpName)

    // Reduce any new multiple internal spaces to single space
    tmpName = normalizeSpaces(tmpName).trim

    tmpName
  }

  /**
   * Produce a normalized version of authority of a taxon name
   * <ol>
   * <li>Perform authority expension of known abbreviated authornames (Linneaus and de Candolle)</li>
   * <li>Recognise "and", "et", "&" as equivalents (special case for "et al.") - all normalized to "&"</li>
   * <li>Remove comma before year, e.g. "Smith 1980" and "Smith, 1980" are equivalents</li>
   * <li>Recognise (e.g.) "F. J. R. Taylor, 1980" and "F.J.R. Taylor, 1980" as equivalents</li>
   * <li>Returns uppercase string, diacritical marks intact
   * </ol>
   *
   * @param name input name presumably genus, genus+species, or genus+species+authority
   * @return normalized version of input name
   */
  def normalizeAuthor(name: String): String = {
    var tmpName: String = name

    // Replace any sequence of white spaces by a single ' ', trim leading/trailing spaces
    tmpName = normalizeSpaces(tmpName).trim

    tmpName = tmpName.replaceAll(""" L\.$""", " Linnaeus")
    tmpName = tmpName.replaceAllLiterally("""(L.)""", "(Linnaeus)")
    tmpName = tmpName.replaceAllLiterally("""L., 1""", "Linnaeus, 1")
    tmpName = tmpName.replaceAllLiterally("""L. 1""", "Linnaeus, 1")
    tmpName = tmpName.replaceAllLiterally("""Linné""", "Linnaeus")
    tmpName = tmpName.replaceAllLiterally("""linné""", "Linnaeus")

    tmpName
  }
}


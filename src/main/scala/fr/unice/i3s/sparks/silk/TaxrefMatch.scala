package fr.unice.i3s.sparks.silk

import java.util.logging.Logger
import java.util.regex.Pattern
import java.util.logging.Level

/**
 * @author Franck Michel, I3S laboratory (this Scala port)
 * @author Olivier Gargominy, INPN (TAXREF-MATCH)
 * @author Tony Rees (orignial version)
 */
object TaxrefMatch {

  val RXSecondTermInRoundBrackets = Pattern.compile("""^([^\(\)\s]+) (\([^\(\)]+\)) (.+)""")
  val RXSecondTermInSquareBrackets = Pattern.compile("""^([^\[\]\s]+) (\[[^\[\]]+\]) (.+)""")
  val RXReplaceAngleBacket = Pattern.compile("""<[^<>]+>""")

  val logger = Logger.getLogger(this.getClass.getName)

  /**
   * Replace multiple occurrences of search with replace in str
   * 
   * No result returned, changes are done on the input StringBuffer parameter
   */
  def replace(str: StringBuffer, search: String, replace: String) {
    var start = str.indexOf(search)
    while (start != -1) {
      str.replace(start, start + search.length, replace)
      start = str.indexOf(search)
    }
  }

  /**
   * Replace any sequence of whitespace characters with a single space.
   * This handles simple white space, tab, CR, LF and non-breaking space
   * 
   * No result returned, changes are done on the input StringBuffer parameter
   */
  def normalizeSpaces(name: StringBuffer) {

    for (idx <- 0 to (name.length - 1)) {
      if (name.charAt(idx) == '\u0020') name.setCharAt(idx, ' ') // space
      if (name.charAt(idx) == '\u0009') name.setCharAt(idx, ' ') // character tabulation
      if (name.charAt(idx) == '\u000b') name.setCharAt(idx, ' ') // line tabulation
      if (name.charAt(idx) == '\u000d') name.setCharAt(idx, ' ') // CR
      if (name.charAt(idx) == '\u000a') name.setCharAt(idx, ' ') // line feed
      if (name.charAt(idx) == '\u00a0') name.setCharAt(idx, ' ') // non-breaking space
    }

    // Remove multiple spaces
    replace(name, "  ", " ")

    // Trim
    if (name.charAt(0) == '\u0020') name.deleteCharAt(0)
    if (name.charAt(name.length - 1) == '\u0020') name.deleteCharAt(name.length - 1)
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
   * 
   * @param name the input name as a StringBuffer
   * @return the updated string
   */
  def extractBracketTerm(name: StringBuffer): String = {
    val matcherRnd = RXSecondTermInRoundBrackets.matcher(name)
    if (matcherRnd.matches)
      return matcherRnd.group(1) + " " + matcherRnd.group(3)

    val matcherSq = RXSecondTermInSquareBrackets.matcher(name)
    if (matcherSq.matches)
      return matcherSq.group(1) + " " + matcherSq.group(3)

    name.toString
  }

  /**
   * Remove all but alphabet letters, spaces, full stops
   * (used on scientific name components only, not authorities) and numbers if with_date is true
   *
   * @param name string in uppercase with normalized spaces
   * @param keepNum whether the keep figures (default true)
   * @return the updated string
   */
  def filterCharacters(name: String, keepNum: Boolean = true): String = {
    name.map(x => x match {
      case c if ('A' <= c && c <= 'Z') => c
      case c if (('0' <= c && c <= '9') && keepNum) => c
      case c if (c == '.' || c == ' ') => c
      case _ => ""
    }).toList.mkString
  }

  /**
   * Normalize the authority part of a taxon name
   * <ol>
   * <li>Perform authority expansion of known abbreviated author names (Linneaus and de Candolle)</li>
   * <li>Recognise "and", "et", "&" as equivalents (special case for "et al.") - all normalized to "&"</li>
   * <li>Remove comma before year, e.g. "Smith 1980" and "Smith, 1980" are equivalents</li>
   * <li>Recognise (e.g.) "F. J. R. Taylor, 1980" and "F.J.R. Taylor, 1980" as equivalents</li>
   * </ol>
   *
   * No result returned, changes are done on the input StringBuffer parameter
   * 
   * @param name input name formatted as <b>uppercase with normalized spaces</b>. Presumably, the name should 
   * represent a genus, genus+species, or genus+species+authority
   */
  def normalizeAuthor(name: StringBuffer) {

    var start = name.lastIndexOf(" L.")
    if (start != -1 && start == name.length - 3)
      // Name ends with "L."
      name.replace(start, name.length, " LINNEAUS")
    else {
      replace(name, "(L.)", " LINNEAUS")
      replace(name, "L., 1", " LINNEAUS 1")
      replace(name, "LINNÉ", " LINNEAUS")
    }

    start = name.lastIndexOf(" D.C.")
    if (start != -1 && start == name.length - 5)
      name.replace(start, name.length, " DE CANDOLLE")
    else {
      replace(name, "(D.C.)", " DE CANDOLLE")
      replace(name, "D.C., 1", " DE CANDOLLE 1")
      replace(name, "D.C. 1", " DE CANDOLLE 1")
    }

    // Normalize "et", "and" to ampersand (leave "et al" as is)
    replace(name, " ET AL", " __ETAL_MARKER__")
    replace(name, " ET ", " & ")
    replace(name, " AND ", " & ")
    replace(name, " __ETAL_MARKER__", " ET AL")

    // Remove commas before dates (only)
    replace(name, ", 17", " 17")
    replace(name, ",17", " 17")
    replace(name, ", 18", " 18")
    replace(name, ",18", " 18")
    replace(name, ", 19", " 19")
    replace(name, ",19", " 19")
    replace(name, ", 20", " 20")
    replace(name, ",20", " 20")

    replace(name, "-", " ")
  }

  /**
   * Produce a normalized version of an input string (scientific name and authority)
   * <ol>
   * <li>Normalize white spaces (cf. normalizeSpaces)</li>
   * <li>Normalize author names (cf. normalizeAuthor)</li>
   * <li>Remove known text elements e.g. 'aff.', 'cf.', 'subsp.', 'subgenus'</li>
   * <li>Remove second term (only) in round or square brackets</li>
   * <li>Replace accented letters with non-accented equivalent letters</li>
   * <li>Remove any chars other than A-Z, 0-9, space, and full stop</li>
   * <li>Remove HTML tags and &AMP;</li>
   * <li>Return uppercase name</li>
   *  </ol>
   * @note This version does not handle genus+author, or genus+species+infraspecies: second "good" term is
   *       presumed to be species epithet, anything after is considered to be start of the authority
   * @note There is a separate function "normalize_auth" for normalizing authorities when required
   *       (e.g. for authority comparisons)
   *
   * @example
   * - Anabaena cf. flos-aquae Ralfs ex Born. et Flah. => ANABAENA FLOSAQUAE RAFLS EX BOEN. ET FLAH.<br>
   * - Abisara lemée-pauli => ABISARA LEMEEPAULI<br>
   * - Fuc/us Vesiculos2us => FUCUS VESICULOSUS<br>
   * - Buffo ignicolor Lacépède, 1788 => BUFFO IGNICOLOR LACEPEDE 1788<br>
   * - Barbatia (Mesocibota) bistrigata (Dunker, 1866) => BARBATIA BISTRIGATA DUNKER 1866<br>
   *
   * @param name input name presumably genus, genus+species, or genus+species+authority
   * @param withDate indicates whether the year is part of the authority (default true). If false,
   * filter out any character 0 to 9.
   * @return normalized version of input name
   */
  def normalize(name: String, withDate: Boolean = true): String = {
    //logger.log(Level.INFO, "Normalizing [" + name + "]")

    var tmpName = new StringBuffer(name.toUpperCase)

    // Replace white spaces and author names, and transform to upper case
    normalizeSpaces(tmpName)
    normalizeAuthor(tmpName)

    // Replace any HTML ampersands
    replace(tmpName, "&AMP;", "&")

    // Replace special characters
    // See list of UTF-16 codes at http://www.fileformat.info/info/charset/UTF-16/list.htm
    replace(tmpName, "\u00df", "BETA") // lowercase beta: ß
    replace(tmpName, "\u03b2", "BETA") // lowercase beta: β
    replace(tmpName, "\u0392", "BETA") // uppercase beta: Β

    replace(tmpName, "\u00c6", "AE") // Æ ligature
    replace(tmpName, "\u0152", "OE") // Œ ligature

    for (idx <- 0 to (tmpName.length - 1)) {
      if (tmpName.charAt(idx) == '\u2026') tmpName.setCharAt(idx, ' ') // ellipsis (three dots in one): …
      if (tmpName.charAt(idx) == '\u00d7') tmpName.setCharAt(idx, ' ') // multiplication sign: ×
      if (tmpName.charAt(idx) == '\u0027') tmpName.setCharAt(idx, ' ') // quote: '
      if (tmpName.charAt(idx) == '\u0022') tmpName.setCharAt(idx, ' ') // quotation mark: " (double quote)
      if (tmpName.charAt(idx) == '\u02bb') tmpName.setCharAt(idx, ' ') // turned comma: ʻ
      if (tmpName.charAt(idx) == '\u02bc') tmpName.setCharAt(idx, ' ') // apostrophe: ʼ
      if (tmpName.charAt(idx) == '\u02bd') tmpName.setCharAt(idx, ' ') // reversed comma: ʽ
      if (tmpName.charAt(idx) == '\u02ca') tmpName.setCharAt(idx, ' ') // acute accent: ˊ (fake apostrophe)
      if (tmpName.charAt(idx) == '\u02cb') tmpName.setCharAt(idx, ' ') // grave accent: ˋ
      if (tmpName.charAt(idx) == '\u02dd') tmpName.setCharAt(idx, ' ') // double acute accent: ˝
      if (tmpName.charAt(idx) == '\u030f') tmpName.setCharAt(idx, ' ') // double grave accent: ̏
      if (tmpName.charAt(idx) == '\u0093') tmpName.setCharAt(idx, ' ') // double turned comma: ̏“
      if (tmpName.charAt(idx) == '\u0094') tmpName.setCharAt(idx, ' ') // double apostrophe: ̏”
      if (tmpName.charAt(idx) == '\u00ab') tmpName.setCharAt(idx, ' ') // left double angle quotation: «
      if (tmpName.charAt(idx) == '\u00bb') tmpName.setCharAt(idx, ' ') // right double angle quotation: »

      // Replace any accented characters
      if (tmpName.charAt(idx) == '\u00c0') tmpName.setCharAt(idx, 'A') // À
      if (tmpName.charAt(idx) == '\u00c1') tmpName.setCharAt(idx, 'A') // Á
      if (tmpName.charAt(idx) == '\u00c2') tmpName.setCharAt(idx, 'A') // Â
      if (tmpName.charAt(idx) == '\u00c3') tmpName.setCharAt(idx, 'A') // Ã
      if (tmpName.charAt(idx) == '\u00c4') tmpName.setCharAt(idx, 'A') // Ä
      if (tmpName.charAt(idx) == '\u00c5') tmpName.setCharAt(idx, 'A') // Å
      if (tmpName.charAt(idx) == '\u00C7') tmpName.setCharAt(idx, 'C') // Ç
      if (tmpName.charAt(idx) == '\u010c') tmpName.setCharAt(idx, 'C') // Č
      if (tmpName.charAt(idx) == '\u0106') tmpName.setCharAt(idx, 'C') // Ć
      if (tmpName.charAt(idx) == '\u0108') tmpName.setCharAt(idx, 'C') // Ĉ
      if (tmpName.charAt(idx) == '\u010a') tmpName.setCharAt(idx, 'C') // Ċ
      if (tmpName.charAt(idx) == '\u010e') tmpName.setCharAt(idx, 'D') // Ď
      if (tmpName.charAt(idx) == '\u0110') tmpName.setCharAt(idx, 'D') // Đ        
      if (tmpName.charAt(idx) == '\u00c8') tmpName.setCharAt(idx, 'E') // È
      if (tmpName.charAt(idx) == '\u00c9') tmpName.setCharAt(idx, 'E') // É
      if (tmpName.charAt(idx) == '\u00ca') tmpName.setCharAt(idx, 'E') // Ê
      if (tmpName.charAt(idx) == '\u00cb') tmpName.setCharAt(idx, 'E') // Ë
      if (tmpName.charAt(idx) == '\u0114') tmpName.setCharAt(idx, 'E') // Ĕ
      if (tmpName.charAt(idx) == '\u00cc') tmpName.setCharAt(idx, 'I') // Ì
      if (tmpName.charAt(idx) == '\u00cd') tmpName.setCharAt(idx, 'I') // Í
      if (tmpName.charAt(idx) == '\u00ce') tmpName.setCharAt(idx, 'I') // Î
      if (tmpName.charAt(idx) == '\u00cf') tmpName.setCharAt(idx, 'I') // Ï
      if (tmpName.charAt(idx) == '\u00d2') tmpName.setCharAt(idx, 'O') // Ò
      if (tmpName.charAt(idx) == '\u00d3') tmpName.setCharAt(idx, 'O') // Ó
      if (tmpName.charAt(idx) == '\u00d4') tmpName.setCharAt(idx, 'O') // Ô
      if (tmpName.charAt(idx) == '\u00d5') tmpName.setCharAt(idx, 'O') // Õ
      if (tmpName.charAt(idx) == '\u00d6') tmpName.setCharAt(idx, 'O') // Ö
      if (tmpName.charAt(idx) == '\u00d8') tmpName.setCharAt(idx, 'O') // Ø
      if (tmpName.charAt(idx) == '\u00d9') tmpName.setCharAt(idx, 'U') // Ù
      if (tmpName.charAt(idx) == '\u00da') tmpName.setCharAt(idx, 'U') // Ú
      if (tmpName.charAt(idx) == '\u00db') tmpName.setCharAt(idx, 'U') // Û
      if (tmpName.charAt(idx) == '\u00dc') tmpName.setCharAt(idx, 'U') // Ü
      if (tmpName.charAt(idx) == '\u016e') tmpName.setCharAt(idx, 'U') // Ů
      if (tmpName.charAt(idx) == '\u00d1') tmpName.setCharAt(idx, 'N') // Ñ
      if (tmpName.charAt(idx) == '\u0160') tmpName.setCharAt(idx, 'S') // Š
      if (tmpName.charAt(idx) == '\u00dd') tmpName.setCharAt(idx, 'Y') // Ý
      if (tmpName.charAt(idx) == '\u0178') tmpName.setCharAt(idx, 'Y') // Ÿ
      if (tmpName.charAt(idx) == '\u017d') tmpName.setCharAt(idx, 'Z') // Ž      
    }

    // Drop indicators of questionable id's, subspecies, varieties
    /* replace(tmpName, " CF ", " ")
    replace(tmpName, " CF. ", " ")
    replace(tmpName, " NEAR ", " ")
    replace(tmpName, " AFF ", " ")
    replace(tmpName, " AFF. ", " ") */
    replace(tmpName, " SP.", " ")
    replace(tmpName, " SPP.", " ")
    replace(tmpName, " SPP ", " ")
    replace(tmpName, " SSP.", " ")
    replace(tmpName, " SUBSP.", " ")
    replace(tmpName, " F. ", " ")
    replace(tmpName, " VAR. ", " ")
    replace(tmpName, " FORM ", " ")
    replace(tmpName, " SUVAR. ", " ")

    // Remove second term if it is surrounded by round or square brackets
    var tmpstr: String = extractBracketTerm(tmpName)

    // Remove any content in angle brackets (e.g. html tags - <i>, </i>, etc.)
    tmpstr = RXReplaceAngleBacket.matcher(tmpstr).replaceAll(" ")

    // Drop any chars other than A-Z, digits if withDate is set, space, and full stop
    tmpstr = filterCharacters(tmpstr, withDate)

    // Reduce any new multiple internal spaces to single space
    tmpName = new StringBuffer(tmpstr)
    normalizeSpaces(tmpName)

    //logger.log(Level.INFO, "Normalized [" + name + "] into [" + tmpName + "]")
    tmpName.toString
  }
}

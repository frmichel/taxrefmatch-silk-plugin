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

  val logger = Logger.getLogger(this.getClass.getName)

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
   * Normalize the authority part of a taxon name
   * <ol>
   * <li>Perform authority expansion of known abbreviated author names (Linneaus and de Candolle)</li>
   * <li>Recognise "and", "et", "&" as equivalents (special case for "et al.") - all normalized to "&"</li>
   * <li>Remove comma before year, e.g. "Smith 1980" and "Smith, 1980" are equivalents</li>
   * <li>Recognise (e.g.) "F. J. R. Taylor, 1980" and "F.J.R. Taylor, 1980" as equivalents</li>
   * </ol>
   *
   * @param name input name presumably genus, genus+species, or genus+species+authority
   * @return name with normalized authority
   */
  def normalizeAuthor(name: String): String = {
    var tmpName = name

    // Replace any sequence of white spaces by a single ' ', trim leading/trailing spaces
    tmpName = normalizeSpaces(tmpName).trim

    tmpName = tmpName.replaceAll(""" L\.$""", " Linnaeus")
    tmpName = tmpName.replace("(L.)", "(Linnaeus)")
    tmpName = tmpName.replace("L., 1", "Linnaeus, 1")
    tmpName = tmpName.replace("L. 1", "Linnaeus, 1")
    tmpName = tmpName.replace("Linné", "Linnaeus")
    tmpName = tmpName.replace("linné", "Linnaeus")

    tmpName = tmpName.replaceAll(""" D\.C\.$""", " de Candolle")
    tmpName = tmpName.replace("(D.C.)", "(de Candolle)")
    tmpName = tmpName.replace("D.C., 1", "de Candolle, 1")
    tmpName = tmpName.replace("D.C. 1", "de Candolle, 1")

    // Normalize "et", "and" to ampersand (leave "et al" as is)
    tmpName = tmpName.replace(" et al", " __ETAL_MARKER__")
    tmpName = tmpName.replace(" et ", " & ")
    tmpName = tmpName.replace(" and ", " & ")
    tmpName = tmpName.replace(" __ETAL_MARKER__", " et al")

    // Remove commas before dates (only)
    tmpName = tmpName.replace(", 17", " 17")
    tmpName = tmpName.replace(",17", " 17")
    tmpName = tmpName.replace(", 18", " 18")
    tmpName = tmpName.replace(",18", " 18")
    tmpName = tmpName.replace(", 19", " 19")
    tmpName = tmpName.replace(",19", " 19")
    tmpName = tmpName.replace(", 20", " 20")
    tmpName = tmpName.replace(",20", " 20")

    tmpName = normalizeSpaces(tmpName).trim
    tmpName = tmpName.replace("-", " ")

    tmpName
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
    var tmpName = name

    // Replace white spaces and author names
    tmpName = normalizeSpaces(tmpName).trim
    tmpName = normalizeAuthor(tmpName).trim
    tmpName = tmpName.toUpperCase

    // Replace any HTML ampersands
    tmpName = tmpName.replace("&AMP;", "&")

    // Replace special characters
    // See list of UTF-16 codes at http://www.fileformat.info/info/charset/UTF-16/list.htm
    tmpName = tmpName.replace("\u00df", "BETA") // lowercase beta: ß
    tmpName = tmpName.replace("\u03b2", "BETA") // lowercase beta: β
    tmpName = tmpName.replace("\u0392", "BETA") // uppercase beta: Β

    tmpName = tmpName.map(x => x match {
      case '\u2026' => ' ' // ellipsis (three dots in one): …
      case '\u00d7' => ' ' // multiplication sign: ×
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
    /* tmpName = tmpName.replace(" CF ", " ")
    tmpName = tmpName.replace(" CF. ", " ")
    tmpName = tmpName.replace(" NEAR ", " ")
    tmpName = tmpName.replace(" AFF ", " ")
    tmpName = tmpName.replace(" AFF. ", " ") */
    tmpName = tmpName.replace(" SP.", " ")
    tmpName = tmpName.replace(" SPP.", " ")
    tmpName = tmpName.replace(" SPP ", " ")
    tmpName = tmpName.replace(" SSP.", " ")
    tmpName = tmpName.replace(" SUBSP.", " ")
    tmpName = tmpName.replace(" F. ", " ")
    tmpName = tmpName.replace(" VAR. ", " ")
    tmpName = tmpName.replace(" FORM ", " ")
    tmpName = tmpName.replace(" SUVAR. ", " ")

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

    // Drop any chars other than A-Z, figures if withDate is set, space, and full stop
    tmpName = filterCharacters(tmpName, withDate)

    // Reduce any new multiple internal spaces to single space
    tmpName = normalizeSpaces(tmpName).trim

    //logger.log(Level.INFO, "Normalized [" + name + "] into [" + tmpName + "]")
    tmpName
  }
}


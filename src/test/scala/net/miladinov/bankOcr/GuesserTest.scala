package net.miladinov.bankOcr

import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

class GuesserTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  "Guesser" should "enumerate all the possible single underscore/pipe removals for the glyph eight" in {
    val eight = """ _ 
                  ||_|
                  ||_|""".stripMargin  

    Guesser.removals(eight).map(_.mkString("\n")) should be (Set(
      """   
        ||_|
        ||_|""".stripMargin,
      """ _ 
        | _|
        ||_|""".stripMargin,
      """ _ 
        || |
        ||_|""".stripMargin,
      """ _ 
        ||_ 
        ||_|""".stripMargin,
      """ _ 
        ||_|
        | _|""".stripMargin,
      """ _ 
        ||_|
        || |""".stripMargin,
      """ _ 
        ||_|
        ||_ """.stripMargin
    ))
  }

  val digitsAndRemovals = Table(
    ("label", "digit", "removals"),
    ("zero",
     """ _ 
       || |
       ||_|""".stripMargin, Set(
     """   
       || |
       ||_|""".stripMargin,
     """ _ 
       |  |
       ||_|""".stripMargin,
     """ _ 
       ||  
       ||_|""".stripMargin,
     """ _ 
       || |
       | _|""".stripMargin,
     """ _ 
       || |
       || |""".stripMargin,
     """ _ 
       || |
       ||_ """.stripMargin
    )),
    ("one",
     """   
       |  |
       |  |""".stripMargin, Set(
     """   
       |   
       |  |""".stripMargin,
     """   
       |  |
       |   """.stripMargin
    )),
    ("two",
     """ _ 
       | _|
       ||_ """.stripMargin, Set(
     """   
       | _|
       ||_ """.stripMargin,
     """ _ 
       |  |
       ||_ """.stripMargin,
     """ _ 
       | _ 
       ||_ """.stripMargin,
     """ _ 
       | _|
       | _ """.stripMargin,
     """ _ 
       | _|
       ||  """.stripMargin
    )),
    ("three",
      """ _ 
        | _|
        | _|""".stripMargin,  Set(
      """   
        | _|
        | _|""".stripMargin,
      """ _ 
        |  |
        | _|""".stripMargin,
      """ _ 
        | _ 
        | _|""".stripMargin,
      """ _ 
        | _|
        |  |""".stripMargin,
      """ _ 
        | _|
        | _ """.stripMargin
    )),
    ("four",
      """   
        ||_|
        |  |""".stripMargin,  Set(
      """   
        | _|
        |  |""".stripMargin,
      """   
        || |
        |  |""".stripMargin,
      """   
        ||_ 
        |  |""".stripMargin,
      """   
        ||_|
        |   """.stripMargin
    )),
    ("five",
      """ _ 
        ||_ 
        | _|""".stripMargin,  Set(
      """   
        ||_ 
        | _|""".stripMargin,
      """ _ 
        | _ 
        | _|""".stripMargin,
      """ _ 
        ||  
        | _|""".stripMargin,
      """ _ 
        ||_ 
        |  |""".stripMargin,
      """ _ 
        ||_ 
        | _ """.stripMargin
    )),
    ("six",
      """ _ 
        ||_ 
        ||_|""".stripMargin,  Set(
      """   
        ||_ 
        ||_|""".stripMargin,
      """ _ 
        | _ 
        ||_|""".stripMargin,
      """ _ 
        ||  
        ||_|""".stripMargin,
      """ _ 
        ||_ 
        | _|""".stripMargin,
      """ _ 
        ||_ 
        || |""".stripMargin,
      """ _ 
        ||_ 
        ||_ """.stripMargin
    )),
    ("seven",
      """ _ 
        |  |
        |  |""".stripMargin,  Set(
      """   
        |  |
        |  |""".stripMargin,
      """ _ 
        |   
        |  |""".stripMargin,
      """ _ 
        |  |
        |   """.stripMargin
    )),
    ("nine",
     """ _ 
       ||_|
       | _|""".stripMargin,  Set(
     """   
       ||_|
       | _|""".stripMargin,
     """ _ 
       | _|
       | _|""".stripMargin,
     """ _ 
       || |
       | _|""".stripMargin,
     """ _ 
       ||_ 
       | _|""".stripMargin,
     """ _ 
       ||_|
       |  |""".stripMargin,
     """ _ 
       ||_|
       | _ """.stripMargin
    ))
  )

  forAll (digitsAndRemovals) { (label: String, glyph: String, expectedRemovals: Set[String]) => {
    it should s"enumerate all the possible single underscore/pipe removals for the glyph $label" in {
      Guesser.removals(glyph).map(_.mkString("\n")) should be (expectedRemovals)
    }
  }}

  val digitsAndAdditions = Table(
    ("label", "digit", "additions"),
    ("zero",
      """ _ 
        || |
        ||_|""".stripMargin, Set(
      """ _ 
        ||_|
        ||_|""".stripMargin
    )),
    ("one",
      """   
        |  |
        |  |""".stripMargin, Set(
      """ _ 
        |  |
        |  |""".stripMargin,
      """   
        || |
        |  |""".stripMargin,
      """   
        | _|
        |  |""".stripMargin,
      """   
        |  |
        || |""".stripMargin,
      """   
        |  |
        | _|""".stripMargin
    )),
    ("two",
      """ _ 
        | _|
        ||_ """.stripMargin, Set(
      """ _ 
        ||_|
        ||_ """.stripMargin,
      """ _ 
        | _|
        ||_|""".stripMargin
    )),
    ("three",
      """ _ 
        | _|
        | _|""".stripMargin,  Set(
      """ _ 
        ||_|
        | _|""".stripMargin,
      """ _ 
        | _|
        ||_|""".stripMargin
    )),
    ("four",
      """   
        ||_|
        |  |""".stripMargin,  Set(
      """ _ 
        ||_|
        |  |""".stripMargin,
      """   
        ||_|
        || |""".stripMargin,
      """   
        ||_|
        | _|""".stripMargin
    )),
    ("five",
      """ _ 
        ||_ 
        | _|""".stripMargin,  Set(
      """ _ 
        ||_|
        | _|""".stripMargin,
      """ _ 
        ||_ 
        ||_|""".stripMargin
    )),
    ("six",
      """ _ 
        ||_ 
        ||_|""".stripMargin,  Set(
      """ _ 
        ||_|
        ||_|""".stripMargin
    )),
    ("seven",
      """ _ 
        |  |
        |  |""".stripMargin,  Set(
      """ _ 
        || |
        |  |""".stripMargin,
      """ _ 
        | _|
        |  |""".stripMargin,
      """ _ 
        |  |
        || |""".stripMargin,
      """ _ 
        |  |
        | _|""".stripMargin
    )),
    ("eight",
      """ _ 
        ||_|
        ||_|""".stripMargin,  Set[String]()
    ),
    ("nine",
      """ _ 
        ||_|
        | _|""".stripMargin,  Set(
      """ _ 
        ||_|
        ||_|""".stripMargin
    ))
  )

  forAll (digitsAndAdditions) { (label: String, glyph: String, expectedAdditions: Set[String]) => {
    it should s"enumerate all the possible single underscore/pipe additions for the glyph $label" in {
      Guesser.additions(glyph).map(_.mkString("\n")) should be (expectedAdditions)
    }
  }}
  
  val digitsAndCorrections = Table(
    ("label", "digit", "corrections"),
    ("zero",
      """ _ 
        || |
        ||_|""".stripMargin, Set(
      """ _ 
        ||_|
        ||_|""".stripMargin
    )),
    ("one",
      """   
        |  |
        |  |""".stripMargin, Set(
      """ _ 
        |  |
        |  |""".stripMargin
    )),
    ("two",
      """ _ 
        | _|
        ||_ """.stripMargin, Set[String]()
    ),
    ("three",
      """ _ 
        | _|
        | _|""".stripMargin,  Set(
      """ _ 
        ||_|
        | _|""".stripMargin
    )),
    ("four",
      """   
        ||_|
        |  |""".stripMargin,  Set[String]()
    ),
    ("five",
      """ _ 
        ||_ 
        | _|""".stripMargin,  Set(
      """ _ 
        ||_|
        | _|""".stripMargin,
      """ _ 
        ||_ 
        ||_|""".stripMargin
    )),
    ("six",
      """ _ 
        ||_ 
        ||_|""".stripMargin,  Set(
      """ _ 
        ||_ 
        | _|""".stripMargin,
      """ _ 
        ||_|
        ||_|""".stripMargin
    )),
    ("seven",
      """ _ 
        |  |
        |  |""".stripMargin,  Set(
      """   
        |  |
        |  |""".stripMargin
    )),
    ("eight",
      """ _ 
        ||_|
        ||_|""".stripMargin,  Set(
      """ _ 
        || |
        ||_|""".stripMargin,
      """ _ 
        ||_ 
        ||_|""".stripMargin,
      """ _ 
        ||_|
        | _|""".stripMargin
    )),
    ("nine",
      """ _ 
        ||_|
        | _|""".stripMargin,  Set(
      """ _ 
        | _|
        | _|""".stripMargin,
      """ _ 
        ||_ 
        | _|""".stripMargin,
      """ _ 
        ||_|
        ||_|""".stripMargin
    ))
  )

  forAll (digitsAndCorrections) { (label: String, glyph: String, expectedCorrections: Set[String]) => {
    it should s"enumerate all the possible corrections for the glyph $label" in {
      Guesser.corrections(glyph).map(_.mkString("\n")) should be (expectedCorrections)
    }
  }}

  val correctionPermutations = Table(
    ("digits", "expectedCorrections"),

    ("0", Set("0", "8")),

    ("8", Set("0", "8", "6", "9")),

    ("34", Set("34", "94")),

    ("01", Set("01", "07", "81", "87")),

    ("949", Set(
      "343", "543", "843", "943",

      "345", "545", "845", "945",

      "348", "548", "848", "948",

      "349", "549", "849", "949"
    )),

    ("999", Set(
      "333", "335", "338", "339", "353", "355", "358", "359",
      "383", "385", "388", "389", "393", "395", "398", "399",

      "533", "535", "538", "539", "553", "555", "558", "559",
      "583", "585", "588", "589", "593", "595", "598", "599",

      "833", "835", "838", "839", "853", "855", "858", "859",
      "883", "885", "888", "889", "893", "895", "898", "899",

      "933", "935", "938", "939", "953", "955", "958", "959",
      "983", "985", "988", "989", "993", "995", "998", "999"
    ))
  )

  forAll (correctionPermutations) { (digits: String, expectedCorrections: Set[String]) => {
    def toGlyphs (digits: String): IndexedSeq[Guesser.Glyph] =
      Parser.linesToGlyphs(Generator.mkGlyph(digits).init.split("\n"))

    val sequenceGlyphs = toGlyphs(digits)
    val expectedCorrectionsGlyphs = expectedCorrections.map(toGlyphs)

    it should s"permute all the possible corrections for the string of digits $digits" in {
      Guesser.permuteCorrections(sequenceGlyphs) should be (expectedCorrectionsGlyphs)
    }
  }}
}

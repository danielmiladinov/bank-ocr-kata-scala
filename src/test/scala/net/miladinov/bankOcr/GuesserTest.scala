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
}

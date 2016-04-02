package net.miladinov.bankOcr

import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

class GuesserTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  "Guesser" should "enumerate all the possible single underscore/pipe removals for the glyph eight" in {
    val eight = """ _ 
                  ||_|
                  ||_|""".stripMargin  

    Guesser.removals(eight) should be (Set(
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
      Guesser.removals(glyph) should be (expectedRemovals)
    }
  }}
}

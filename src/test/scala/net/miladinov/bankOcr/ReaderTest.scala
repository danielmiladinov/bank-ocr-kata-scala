package net.miladinov.bankOcr

import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source

class ReaderTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  
  "Reader" should "read an input file containing many account numbers" in {
    val actualDigits = Reader.read(testFile("/multiple-lines.txt"))

    val expectedDigits = formatted(
      """ _  _  _  _  _  _  _  _  _ 
        || || || || || || || || || |
        ||_||_||_||_||_||_||_||_||_|""",
      """                           
        |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |""",
      """ _  _  _  _  _  _  _  _  _ 
        | _| _| _| _| _| _| _| _| _|
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ """,
      """ _  _  _  _  _  _  _  _  _ 
        | _| _| _| _| _| _| _| _| _|
        | _| _| _| _| _| _| _| _| _|""",
      """                           
        ||_||_||_||_||_||_||_||_||_|
        |  |  |  |  |  |  |  |  |  |""",
      """ _  _  _  _  _  _  _  _  _ 
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
        | _| _| _| _| _| _| _| _| _|""",
      """ _  _  _  _  _  _  _  _  _ 
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
        ||_||_||_||_||_||_||_||_||_|""",
      """ _  _  _  _  _  _  _  _  _ 
        |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |""",
      """ _  _  _  _  _  _  _  _  _ 
        ||_||_||_||_||_||_||_||_||_|
        ||_||_||_||_||_||_||_||_||_|""",
      """ _  _  _  _  _  _  _  _  _ 
        ||_||_||_||_||_||_||_||_||_|
        | _| _| _| _| _| _| _| _| _|"""
    )

    actualDigits shouldEqual expectedDigits
  }
  
  val filesAndExpectedOutputs = Table(
    ("input file", "expectedOutput"),
    ("/123456789.txt",
      """    _  _     _  _  _  _  _ 
        |  | _| _||_||_ |_   ||_||_|
        |  ||_  _|  | _||_|  ||_| _|"""
    ),
    ("/000000000.txt",
      """ _  _  _  _  _  _  _  _  _ 
        || || || || || || || || || |
        ||_||_||_||_||_||_||_||_||_|"""
    ),
    ("/111111111.txt",
      """                           
        |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |"""
    ),
    ("/222222222.txt",
      """ _  _  _  _  _  _  _  _  _ 
        | _| _| _| _| _| _| _| _| _|
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ """
    ),
    ("/333333333.txt",
      """ _  _  _  _  _  _  _  _  _ 
        | _| _| _| _| _| _| _| _| _|
        | _| _| _| _| _| _| _| _| _|"""
    ),
    ("/444444444.txt",
      """                           
        ||_||_||_||_||_||_||_||_||_|
        |  |  |  |  |  |  |  |  |  |"""
    ),
    ("/555555555.txt",
      """ _  _  _  _  _  _  _  _  _ 
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
        | _| _| _| _| _| _| _| _| _|"""
    ),
    ("/666666666.txt",
      """ _  _  _  _  _  _  _  _  _ 
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
        ||_||_||_||_||_||_||_||_||_|"""
    ),
    ("/777777777.txt",
      """ _  _  _  _  _  _  _  _  _ 
        |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |"""
    ),
    ("/888888888.txt",
      """ _  _  _  _  _  _  _  _  _ 
        ||_||_||_||_||_||_||_||_||_|
        ||_||_||_||_||_||_||_||_||_|"""
    ),
    ("/999999999.txt",
      """ _  _  _  _  _  _  _  _  _ 
        ||_||_||_||_||_||_||_||_||_|
        | _| _| _| _| _| _| _| _| _|"""
    )
  )
  
  forAll (filesAndExpectedOutputs) { (fileName: String, expectedOutput: String) => {
    it should s"be able to convert the contents of $fileName to $expectedOutput" in {
      val actualDigits = Reader.read(testFile(fileName))

      actualDigits shouldEqual formatted(expectedOutput)
    }
  }}

  private def testFile (fileName: String) = Source.fromInputStream(
    getClass.getResourceAsStream(fileName)
  )
  
  private def formatted (lines: String*) = {
    lines.map(_.stripMargin.split("\n").toVector).toVector
  }
}

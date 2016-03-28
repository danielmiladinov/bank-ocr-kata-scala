package net.miladinov.bankOcr

import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source

class BankOcrReaderTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  
  "BankOcrReader" should "read an input file containing many account numbers" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/multiple-lines.txt"))

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
    it should s"be able to read the contents of $fileName containing the representation of ${fileName.filter(_.isDigit)}" in {
      val ocr = new BankOcrReader()
      val actualDigits = ocr.read(testFile(fileName))

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

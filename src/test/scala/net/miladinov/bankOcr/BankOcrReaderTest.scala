package net.miladinov.bankOcr

import org.scalatest.{FlatSpec, ShouldMatchers}

import scala.io.Source

class BankOcrReaderTest extends FlatSpec with ShouldMatchers {

  "A BankOcrReader" should "read and parse an input file containing 123456789" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/123456789.txt"))
    
    val expectedDigits = formatted {
      """    _  _     _  _  _  _  _ 
        |  | _| _||_||_ |_   ||_||_|
        |  ||_  _|  | _||_|  ||_| _|"""
    }
    
    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 111111111" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/111111111.txt"))

    val expectedDigits = formatted {
      """                           
        |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |"""
    }

    actualDigits shouldEqual expectedDigits
  }

  it should "read and parse an input file containing 222222222" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/222222222.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        | _| _| _| _| _| _| _| _| _|
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ """
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 333333333" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/333333333.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        | _| _| _| _| _| _| _| _| _|
        | _| _| _| _| _| _| _| _| _|"""
    }

    actualDigits shouldEqual expectedDigits
  }

  it should "read and parse an input file containing 444444444" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/444444444.txt"))

    val expectedDigits = formatted {
      """                           
        ||_||_||_||_||_||_||_||_||_|
        |  |  |  |  |  |  |  |  |  |"""
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 555555555" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/555555555.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
        | _| _| _| _| _| _| _| _| _|"""
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 666666666" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/666666666.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
        ||_||_||_||_||_||_||_||_||_|"""
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 777777777" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/777777777.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |"""
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 888888888" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/888888888.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        ||_||_||_||_||_||_||_||_||_|
        ||_||_||_||_||_||_||_||_||_|"""
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 999999999" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/999999999.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        ||_||_||_||_||_||_||_||_||_|
        | _| _| _| _| _| _| _| _| _|"""
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing 000000000" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/000000000.txt"))

    val expectedDigits = formatted {
      """ _  _  _  _  _  _  _  _  _ 
        || || || || || || || || || |
        ||_||_||_||_||_||_||_||_||_|"""
    }

    actualDigits shouldEqual expectedDigits
  }
  
  it should "read and parse an input file containing many account numbers" in {
    val ocr = new BankOcrReader()
    val actualDigits = ocr.read(testFile("/multiple-lines.txt"))
    
    val expectedDigits = formatted {
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
    }
    
    actualDigits shouldEqual expectedDigits
  }
  
  private def testFile (fileName: String) = Source.fromInputStream(
    getClass.getResourceAsStream(fileName)
  )
  
  private def formatted (lines: String*) = {
    lines.map(_.stripMargin.split("\n").toVector).toVector
  }
}

package net.miladinov.bankOcr

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, ShouldMatchers}

class ValidatorTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  "Validator" should "validate a valid account number" in {
    Validator.validate("345882865") should be (true)
  }

  val numbers = Table(
    ("account number", "valid?"),
    ("457508000", true),
    ("664371495", false),
    ("64371495", false),
    ("6437149567", false)
  )

  forAll (numbers) { (accountNumber: String, expectedValidation: Boolean) => {
    it should s"validate $accountNumber as $expectedValidation" in {
      Validator.validate(accountNumber) should be (expectedValidation)
    }
  }}
}

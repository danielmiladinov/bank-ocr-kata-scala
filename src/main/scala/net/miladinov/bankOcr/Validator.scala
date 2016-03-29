package net.miladinov.bankOcr

object Validator {
  final val VALID_ACCOUNT_NUMBER_LENGTH = 9

  def validate (s: String): Boolean = s.length == VALID_ACCOUNT_NUMBER_LENGTH && {
    val Array(d9, d8, d7, d6, d5, d4, d3, d2, d1) = s.split("").map(_.toInt)
    (d1 + 2*d2 + 3*d3 + 4*d4 + 5*d5 + 6*d6 + 7*d7 + 8*d8 + 9*d9) % 11 == 0
  }
}

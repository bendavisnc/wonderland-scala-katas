package alphabet_cipher

import alphabet_cipher.Coder.*

class CoderTest extends munit.FunSuite {

  test("can encode a message with a secret keyword") {
    assertEquals(encode("vigilance", "meetmeontuesdayeveningatseven"),
                 "hmkbxebpxpmyllyrxiiqtoltfgzzv"
    )
    assertEquals(encode("scones", "meetmebythetree"), "egsgqwtahuiljgs")
  }

  test("can decode a message given an encoded message and a secret keyword") {
    assertEquals(decode("vigilance", "hmkbxebpxpmyllyrxiiqtoltfgzzv"),
                 "meetmeontuesdayeveningatseven"
    )
    assertEquals(decode("scones", "egsgqwtahuiljgs"), "meetmebythetree")
  }

  test("can extract the secret keyword given an encrypted message and the original message") {
    assertEquals(decipher("opkyfipmfmwcvqoklyhxywgeecpvhelzg", "thequickbrownfoxjumpsoveralazydog"),
                 "vigilance"
    )
    assertEquals(decipher("hcqxqqtqljmlzhwiivgbsapaiwcenmyu", "packmyboxwithfivedozenliquorjugs"),
                 "scones"
    )
    assertEquals(decipher("hfnlphoontutufa", "hellofromrussia"), "abcabcx")
  }

}

import java.io.{BufferedInputStream, FileInputStream}
import java.security.MessageDigest


def hexEncode(in: Array[Byte]): String = {
  val sb = new StringBuilder
  val len = in.length
  def addDigit(in: Array[Byte], pos: Int, len: Int, sb: StringBuilder) {
    if (pos < len) {
      val b: Int = in(pos)
      val msb = (b & 0xf0) >> 4
      val lsb = (b & 0x0f)
      sb.append((if (msb < 10) ('0' + msb).asInstanceOf[Char] else ('a' + (msb - 10)).asInstanceOf[Char]))
      sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[Char] else ('a' + (lsb - 10)).asInstanceOf[Char]))

      addDigit(in, pos + 1, len, sb)
    }
  }
  addDigit(in, 0, len, sb)
  sb.toString
}
/** create a MD5 digest from a Byte array */

val fileName = """/home/dev/svn-root/trunk/csv-validator/csv-validator-core/src/test/resources/uk/gov/tna/dri/schema/checksum.txt"""

val digest = MessageDigest.getInstance("GYU")

val file = new BufferedInputStream(new FileInputStream(fileName))
Stream.continually(file.read).takeWhile(-1 !=).map(_.toByte).foreach( digest.update(_))
val hexStr = hexEncode(digest.digest)
val same = hexStr == "699d61aff25f16a5560372e610da91ab"
println( s"Hex: $hexStr  $same")
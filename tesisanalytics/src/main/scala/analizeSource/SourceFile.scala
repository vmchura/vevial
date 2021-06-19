package analizeSource

import java.io.{File, FileInputStream}

case class SourceFile private (inputFile: File, hashID: String) {
  override def equals(obj: Any): Boolean =
    obj match {
      case SourceFile(_, hashOther) => hashOther.equals(hashID)
      case _                        => false
    }
}

object SourceFile {
  def apply(file: File): SourceFile = new SourceFile(file, md5HashString(file))
  def md5HashString(file: File): String = {
    import java.security.MessageDigest
    import java.math.BigInteger
    val md = MessageDigest.getInstance("MD5")
    val in = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    in.read(bytes)
    in.close()
    val digest = md.digest(bytes)
    val bigInt = new BigInteger(1, digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }
}

package analizeSource

import io.vmchura.vevial.PlanarGeometric.BasicEje.EfficientSeqEjeElements
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.EjeElement.RectSegment
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI

import java.io.{File, FileInputStream}
case class SourceFile private (
    inputFile: File,
    hashID: String,
    inOpt: Option[TPoint],
    outOpt: Option[TPoint]
) {
  override def equals(obj: Any): Boolean =
    obj match {
      case SourceFile(_, hashOther, _, _) => hashOther.equals(hashID)
      case _                              => false
    }
  def relevamiento: RelevamientoIRI[IRIElementData] =
    RelevamientoIRI(inputFile, cd => IRIElementData(cd))
  def buildEje(): Option[EfficientSeqEjeElements] = {
    val elements =
      relevamiento.elements
        .zip(relevamiento.elements.tail)
        .flatMap {
          case (i, j) =>
            for {
              pi <- i.point
              pj <- j.point
            } yield {
              RectSegment(pi.value, pj.value)
            }
        }
        .toList
    //500km
    Option.when(elements.map(_.length).sum < 500e3)(
      EfficientSeqEjeElements(elements)
    )
  }

  override def toString: String = s"${inputFile.getPath}"
}

object SourceFile {
  def apply(file: File): SourceFile = {
    val rel = RelevamientoIRI(file, cd => IRIElementData(cd))
    new SourceFile(
      file,
      md5HashString(file),
      rel.elements.find(_.point.isDefined).flatMap(_.point.map(_.value)),
      rel.elements.findLast(_.point.isDefined).flatMap(_.point.map(_.value))
    )
  }
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

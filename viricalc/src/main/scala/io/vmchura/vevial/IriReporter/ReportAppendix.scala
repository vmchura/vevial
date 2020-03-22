package io.vmchura.vevial.IriReporter

import java.nio.file.{Files, Path}

import com.norbitltd.spoiwo.model.enums.{CellBorderStyle, CellFill, CellHorizontalAlignment, CellVerticalAlignment, PaperSize}
import com.norbitltd.spoiwo.model._
import com.norbitltd.spoiwo.natures.xlsx.Model2XlsxConversions._

import scala.collection.mutable.ListBuffer
object ReportAppendix {
  val heightImageRows: Int = 14


  def buildAppendix(respaldo: Seq[TRespaldoByImages],pathHeader: String,tramoTag: String): Option[Path] = {

    try {
      val respaldoSorted: Seq[(TRespaldoByImages, TImage)] = respaldo.sortBy(_.progIni).flatMap { r => r.images.map(i => (r, i)) }
      val (sh,sb) = respaldoSorted.foldLeft((List.empty[Sheet], new SheetBuilder(pathHeader, tramoTag, 1))) {
        case ((prev, sb), (respaldo, image)) => {
          if (sb.isFull()) {
            val newSB = new SheetBuilder(pathHeader, tramoTag, sb.hojaNum + 1)
            newSB.addImage(respaldo, image)
            (sb.buildSheet() :: prev, newSB)
          } else {
            sb.addImage(respaldo, image)
            (prev, sb)
          }
        }
      }

      val sheets = (sb.buildSheet() :: sh).reverse


      val path = Files.createTempFile("report", ".xlsx")

      Workbook().withSheets(sheets).saveAsXlsx(path.toAbsolutePath.toString)
      Some(path)

    }catch{
      case e:Throwable => {
        println(e)
        None
      }
    }
  }
  private class SheetBuilder(pathHeader: String, tramoTag: String,val hojaNum: Int){
    val rowsIndexed = ListBuffer.empty[Row]
    val ih = Image(CellRange(0 -> 0, 0 -> 9), pathHeader)
    val otherImages = ListBuffer.empty[Image]
    val mergedRegions = ListBuffer.empty[CellRange]
    val styleTag =  CellStyle(borders = Reporter.bordersItem)
    val styleProg =  CellStyle(borders = Reporter.bordersItem,dataFormat = CellDataFormat("0+000"))
    rowsIndexed.append(Row(index = 0,height = new Height(40,HeightUnit.Point)))
    val cellsTramo = Seq(Cell.Empty,Cell("Tramo:", style = styleTag),Cell(tramoTag, style = styleTag.withHorizontalAlignment(CellHorizontalAlignment.Right)))
    rowsIndexed.append(Row(index = 1,height = new Height(20,HeightUnit.Point)).withCells(cellsTramo))
    mergedRegions.append(CellRange(1 -> 1, 2 ->8))

    def length(): Int = otherImages.length
    def isFull(): Boolean = length() > 1



    def addImage(respaldo: TRespaldoByImages, image: TImage): Unit = {
      if(isFull()){
        throw  new IllegalStateException("adding image when is full")
      }
      val startRowImage = 3+length()*(heightImageRows+4)
      val startRowDesc = startRowImage+heightImageRows
      //          Cell(resultLabel.progIni,style = CellStyle(borders = Reporter.bordersItem,dataFormat = CellDataFormat("0+000")).addColor(indx)),

      val cellsDescription = Seq(Cell.Empty,Cell("Descripción:",style = styleTag),Cell(image.caption,style = styleTag))
      rowsIndexed.append(Row(index = startRowDesc,height = new Height(20,HeightUnit.Point)).withCells(cellsDescription))

      val cellsPuntoCritico = Seq(Cell.Empty,Cell("Pto.Crit - Prog.Inicio:",style = styleTag),
        Cell(respaldo.progIni,style = styleProg),
        Cell("Pto.Crit - Prog. Fin:",style = styleTag),
        Cell(respaldo.progFin,style = styleProg),
      )
      rowsIndexed.append(Row(index = startRowDesc+1,height = new Height(20,HeightUnit.Point)).withCells(cellsPuntoCritico))

      val cellsImagen = Seq(Cell.Empty,Cell("Imagen Prog:",style = styleTag),
        Cell(image.prog(),style = styleProg),
        Cell("Latitud:",style = styleTag),
        Cell(image.lat,style = styleTag),
        Cell("Longitud:",style = styleTag),
        Cell(image.lng,style = styleTag)
      )
      rowsIndexed.append(Row(index = startRowDesc+2,height = new Height(20,HeightUnit.Point)).withCells(cellsImagen))

      otherImages.append(Image(CellRange(startRowImage -> startRowDesc, 1 -> 8),image.path))
      mergedRegions.append(CellRange(startRowDesc -> startRowDesc, 2 ->8))
      mergedRegions.append(CellRange(startRowImage -> (startRowDesc-1), 1 ->7))
      rowsIndexed.append(Row(index = startRowImage).withCells(Cell.Empty,Cell("Imagen").withStyle(CellStyle(borders = Reporter.bordersItemTHICK))))

    }
    val marginWidth = 200
    val tagWidth = 4000
    val dataWidth =  1200
    def buildSheet(): Sheet = {Sheet(name = s"Hoja$hojaNum", images = (otherImages.append(ih)).toList).withRows(rowsIndexed.toList).
      withColumns(
        Column(width = new Width(marginWidth, WidthUnit.Unit)),
        Column(width = new Width(tagWidth, WidthUnit.Unit)),
        Column(width = new Width(dataWidth, WidthUnit.Unit)),
        Column(width = new Width(tagWidth, WidthUnit.Unit)),
        Column(width = new Width(dataWidth, WidthUnit.Unit)),
        Column(width = new Width(tagWidth, WidthUnit.Unit)),
        Column(width = new Width(dataWidth, WidthUnit.Unit)),
        Column(width = new Width(marginWidth, WidthUnit.Unit)),
        Column(width = new Width(marginWidth, WidthUnit.Unit)),
        Column(width = new Width(marginWidth, WidthUnit.Unit))
      ).withMergedRegions(mergedRegions.toList).withoutMargins
    }
  }


}

package io.vmchura.vevial.IriReporter

import java.nio.file.{Files, Path}

import com.norbitltd.spoiwo.model.enums.{CellBorderStyle, CellFill, CellHorizontalAlignment, CellVerticalAlignment}
import com.norbitltd.spoiwo.model._
import io.vmchura.vevial.Calculator.IriCalculator
import io.vmchura.vevial.Calculator.models.RelevamientoIRIProgresivas
import io.vmchura.vevial.EjeVialUtil.Progresiva
import com.norbitltd.spoiwo.natures.xlsx.Model2XlsxConversions._
import io.vmchura.vevial.IriReporter.Reporter.MorningColor._
import io.vmchura.vevial.elementdata.IRIElementData

import scala.collection.mutable.ListBuffer

class Reporter(source: Seq[RelevamientoIRIProgresivas[IRIElementData]],
               minProg: Int, maxProg: Int, interval: Int,
               calcD: Int => Double,
               pathHeader: String,tramoTag: String) {

  val carrilIzquierdoData: ListBuffer[IriValueAfterProcess] = ListBuffer.empty
  val carrilDerechoData: ListBuffer[IriValueAfterProcess] = ListBuffer.empty


  private def addDataToLeft(data: Seq[IriValueAfterProcess]): Unit = carrilIzquierdoData ++= data
  private def addDataToRight(data: Seq[IriValueAfterProcess]): Unit = carrilDerechoData ++= data
  def addData(data: Seq[IriValueAfterProcess], toLeft: Boolean): Unit = {
    if(toLeft) addDataToLeft(data) else addDataToRight(data)
  }


  def createSheetWithCarrilData(relevamientos: Seq[IriValueAfterProcess], huella: String): Sheet = {
    val sorted = relevamientos.sortBy(_.progresiva)

    /**
      * Row().withCellValues("Item","Progresiva","IRI","FileTag","FileItem","Description","observaciones") +:
      */
    val rowsSelected: Seq[Row] = sorted.zipWithIndex.map { case (e,indx) =>
      val seqCells = Seq(Cell(indx + 1),
        Cell(e.progresiva).withStyle(CellStyle(dataFormat = CellDataFormat("0+000"))),
        e.iriValue.map(v => Cell(v)).getOrElse(Cell.Empty),
        Cell(s"${e.fileTag}[${e.itemOnFile}]"),
        e.description.map(desc => Cell(desc)).getOrElse(Cell.Empty))

      Row().withCells(seqCells)

    }

    /**
      * images =(1 to 8).map(i =>
      * Image(CellRange(i -> (i+1), 1 -> i), "examples/src/main/resources/einstein.jpg")
      * ).toList
      */
    Sheet(name = "Huella" + huella, images=List(Image(CellRange(1 -> 1,1->10),pathHeader))).withRows(
      List(Row()) ++ rowsSelected)


  }
  type CellName = String
  case class ResultCarril(prom: CellName, desv: CellName, d: CellName,iric: CellName){
    override def toString: CellName = s"[$prom/$desv/$d/$iric]"
  }
  case class ResultsLabel(sheet: Sheet,progIni: Int, progFin: Int, left: ResultCarril, right: ResultCarril, iric: CellName){
    override def toString: CellName = s"$progIni $left $right $iric"
  }


  val styleColumnHeader = CellStyle(fillForegroundColor = color3,
    fillPattern = CellFill.Solid,
    borders = Reporter.bordersItem,
    horizontalAlignment = CellHorizontalAlignment.Center,
    verticalAlignment = CellVerticalAlignment.Center
  )
  val styleCarrilHeader = styleColumnHeader.copy(borders = Some(Reporter.bordersItemTHICK), fillForegroundColor = Some(color1))
  val labelDataStyle = styleColumnHeader.copy(horizontalAlignment = Some(CellHorizontalAlignment.Left), fillForegroundColor = Some(color2))
  val dataStyle = styleColumnHeader.copy(horizontalAlignment = Some(CellHorizontalAlignment.Right), fillForegroundColor = Some(color5))
  val aboutSheetStyle = styleCarrilHeader.copy(fillForegroundColor = Some(color1))


  def buildHeaderRows(secction: String,withRef: Boolean) = List(
    Row().withHeight(height = new Height(40,HeightUnit.Point)),
    Row(cells = Seq(Cell("Rugosidad", style= aboutSheetStyle))),
    Row(cells = Seq(Cell("International Roughness Index (IRI)",style= aboutSheetStyle))),
    Row(cells = Seq(Cell("Tramo",style= labelDataStyle),Cell.Empty,Cell(tramoTag,style= dataStyle))),
    //Row(cells = Seq(Cell("Sección",style= labelDataStyle),Cell.Empty,Cell(,style= dataStyle))),
    Row(cells = Seq(Cell("Sección",style= labelDataStyle),Cell.Empty,Cell(secction,style= dataStyle))),
    Row(cells = Seq(Cell("Fecha",style= labelDataStyle),Cell.Empty,
      (if(withRef) Cell(s"=Resumen!C6") else Cell.Empty).withStyle(dataStyle))),
    Row(cells = Seq(Cell("Equipo",style= labelDataStyle),Cell.Empty,
      (if(withRef) Cell(s"=Resumen!C7") else Cell.Empty).withStyle(dataStyle))),

    Row(cells = Seq(Cell("Ing resp.",style= labelDataStyle),Cell.Empty,
      (if(withRef) Cell(s"=Resumen!C8") else Cell.Empty).withStyle(dataStyle)))



  )
  def buildResumenFilesUsed(): Sheet = {
    import com.norbitltd.spoiwo.model.Height._
    val r0 =  Row().withHeight(height = new Height(40,HeightUnit.Point))
    val r1 =  Row(cells = Seq(Cell("Descripción de los archivos usados para generar el presente reporte",
      style= CellStyle(horizontalAlignment = CellHorizontalAlignment.Center,
        font = Font(bold = true,height = 16.points),
        fillForegroundColor = color1,
        fillPattern = CellFill.Solid)))
      , height = 45.points)

    val r2 = Row(cells = Seq(Cell(
      "El presente reporte ha utilizado uno o más archivos fuente, para indicar una referencia a los datos de cada uno se ha asignado una letra mayúscula a cada uno como indentificador." +
      "\nComo cada archivo fuente tiene un encabezado único a continuación se señala el identificador (letra mayúscula) que se asignó al archivo que contenga el encabezado correspondiente",
      style= CellStyle(horizontalAlignment = CellHorizontalAlignment.Left,
        verticalAlignment = CellVerticalAlignment.Center,
        font = Font(italic = true),
        wrapText = true,
        fillForegroundColor = color2,
        fillPattern = CellFill.Solid)
    )), height = 90.points)

    val emptyRow: Row = Row(height= 5.points).withCells(Seq.fill(9)(Cell.Empty.withStyle(CellStyle(fillForegroundColor = Color.Black,fillPattern = CellFill.Solid))))
    val blankRow: Row = Row(height= 5.points).withCells(Seq.fill(9)(Cell.Empty.withStyle(CellStyle(fillForegroundColor = Color.White,fillPattern = CellFill.Solid))))
    val rowsFile = source.map{ e =>
      Seq(blankRow,emptyRow,
        Row(cells = Seq(Cell(e.fileID,
        style=CellStyle(horizontalAlignment = CellHorizontalAlignment.Center,
          verticalAlignment = CellVerticalAlignment.Center,
          font = Font(bold = true,height =  25.points),
          fillForegroundColor = color3,
          fillPattern = CellFill.Solid,
          borders = Reporter.bordersItemTHICK))),height = 30.points)) ++
      e.relevamientoIRI.header.map(items =>

        Row().withCells(items.map{ item =>
          Cell(item)
        })) ++ Seq(emptyRow)
    }
    val rowsTagsFile = rowsFile.scanLeft(5){case (prev,seqRows) => prev+seqRows.length}.dropRight(1)
    Sheet(name="Encabezados",rows = (Seq(r0,r1,r2) ++ rowsFile.flatten).toList,
      images=List(Image(CellRange(0 -> 0,0->9),pathHeader))
    ).withMergedRegions(
      List(CellRange(1 -> 1, 0 ->8),
      CellRange(2 -> 2, 0 ->8)) ++
        rowsTagsFile.map(indx => CellRange(indx -> indx, 0 -> 8))).
      withColumns(Column(width = new Width(22, WidthUnit.Character)))


  }

  def createResumenSheet(results: Seq[ResultsLabel]): List[Sheet] = {
    if(results.isEmpty){
      Nil
    }else{
      val minProg = results.minBy(_.progIni).progIni
      val maxProg = results.maxBy(_.progFin).progFin
      val secctionStr  = s"Del km ${Progresiva(minProg).show(withSpaces = true,0)} al km ${Progresiva(maxProg).show(withSpaces = true,0)}"
      val cellsHeader = List("Item","Progresiva\nInicio","Progresiva\nFin").map(s => Cell(s).withStyle(styleColumnHeader))

      def carrilHeaders(huella: String) = List("IRI\nprom","Desv\nStd","D",s"IRIc\n($huella)").map(s => Cell(s).withStyle(styleColumnHeader.copy(wrapText = Some(true))))

      val internalHeader = Seq(
        Row(cells = List(
          Cell.Empty,Cell.Empty,Cell.Empty,
          Cell("Huella Izquierda").withStyle(styleColumnHeader),Cell.Empty,Cell.Empty,Cell.Empty,
          Cell("Huella Derecha").withStyle(styleColumnHeader),Cell.Empty,Cell.Empty,Cell.Empty,
          Cell("Ambos").withStyle(styleColumnHeader)
        ), height = new Height(21,HeightUnit.Point)),

        Row(cells = cellsHeader ++ carrilHeaders("izq") ++ carrilHeaders("der")++List(Cell("IRIc").withStyle(styleColumnHeader.copy(fillForegroundColor = Some(color1)))),
          height = new Height(30,HeightUnit.Point)
        )

      )
      val styleNum = CellStyle(dataFormat = CellDataFormat("0.00;-0.00;—;@"), borders = Reporter.bordersItem)
      implicit class CellStyleSwapColor(celltyle: CellStyle){
        def buildColorFromIndx(indx: Int): Color = if(indx%2==1) color6 else color4
        def addColor(indx: Int): CellStyle = celltyle.copy(fillForegroundColor = Some(buildColorFromIndx(indx)),fillPattern = Some(CellFill.Solid))
      }
      def resumenCarrilToRows(sheetName: Option[String],resultCarril: ResultCarril, indx: Int): Seq[Cell] = {
        val styleToRow = styleNum.addColor(indx)
        sheetName.map{ name =>

          Seq(
            Cell(s"=$name!${resultCarril.prom}", style = styleToRow),
            Cell(s"=$name!${resultCarril.desv}", style = styleToRow),
            Cell(s"=$name!${resultCarril.d}", style = styleToRow),
            Cell(s"=$name!${resultCarril.iric}", style = styleToRow),
          )

        }.getOrElse(Seq.fill(4)(Cell.Empty.withStyle(styleToRow)))

      }



      val rowsResumen = results.sortBy(_.progIni).zipWithIndex.map{ case (resultLabel,indx) =>
        Row(cells = Seq(
          Cell(indx+1, style = CellStyle(borders = Reporter.bordersItem).addColor(indx)),
          Cell(resultLabel.progIni,style = CellStyle(borders = Reporter.bordersItem,dataFormat = CellDataFormat("0+000")).addColor(indx)),
          Cell(resultLabel.progFin,style = CellStyle(borders = Reporter.bordersItem,dataFormat = CellDataFormat("0+000")).addColor(indx))
          ) ++ resumenCarrilToRows(resultLabel.sheet.name,resultLabel.left,indx) ++
           resumenCarrilToRows(resultLabel.sheet.name,resultLabel.right,indx) ++ {
          resultLabel.sheet.name.map{ shName =>
            List(Cell(s"=$shName!${resultLabel.iric}",style = styleNum.addColor(indx)))
          }.getOrElse(List(Cell.Empty.withStyle(styleNum.addColor(indx))))
        }
        )
      }

      List(Sheet(name ="Resumen",
        rows = buildHeaderRows(secctionStr,withRef = false) ++ internalHeader ++ rowsResumen,
        images=List(Image(CellRange(0 -> 0,0->12),pathHeader))
       ).withMergedRegions(
        CellRange(1 -> 1, 0 ->11),
        CellRange(2 -> 2, 0 ->11),
        CellRange(3 -> 3, 0 ->1),
        CellRange(3 -> 3, 2 ->11),
        CellRange(4 -> 4, 0 ->1),
        CellRange(4 -> 4, 2 ->11),

        CellRange(5 -> 5, 0 ->1),
        CellRange(5 -> 5, 2 ->11),
        CellRange(6 -> 6, 0 ->1),
        CellRange(6 -> 6, 2 ->11),
        CellRange(7 -> 7, 0 ->1),
        CellRange(7 -> 7, 2 ->11),

        CellRange(8 -> 8, 3 ->6),
        CellRange(8 -> 8, 7 ->10),
      ).withColumns((0 to 11).toList.map{
        case 0 => Column(width = new Width(4,measureUnit = WidthUnit.Character))
        case 1 => Column(width = new Width(8,measureUnit = WidthUnit.Character))
        case 2 => Column(width = new Width(8,measureUnit = WidthUnit.Character))
        case _ => Column(width = new Width(5,measureUnit = WidthUnit.Character))
      })
      )
    }

  }
  def createSheetsByInterval(): Seq[ResultsLabel] = {
    import Reporter.MorningColor._
    val ciData = carrilIzquierdoData.toList.sortBy(_.progresiva)
    val cdData = carrilDerechoData.toList.sortBy(_.progresiva)
    IriCalculator.genIntervals(minProg,maxProg,interval).map{ case (inclusive,exclusive) =>


      val ci = ciData.filter(e => inclusive <= e.progresiva && e.progresiva < exclusive)
      val cd = cdData.filter(e => inclusive <= e.progresiva && e.progresiva < exclusive)
      val cellsHeader = List("Item","Progresiva","IRI","Archivo[Item]","Observaciones").map(s => Cell(s).withStyle(styleColumnHeader))
      val middleCell = Cell.Empty.withStyle(style = CellStyle(fillForegroundColor = color4, fillPattern = CellFill.Solid))

      val internalHeader = Seq(
        Row(cells = List(Cell("Huella Izquierda").withStyle(styleColumnHeader),Cell.Empty,Cell.Empty,Cell.Empty,Cell.Empty
        ,middleCell,
        Cell("Huella Derecha").withStyle(styleColumnHeader))),

        Row(cells = cellsHeader ++ List(middleCell) ++ cellsHeader))







      val offsetRows = 8
      val offsetPage = 0
      val offsetMiddle = 1
      val offsetToIri = 2
      val columnsPerCarril = 5
      val colLeft = ('A'+(offsetPage+offsetToIri)).toChar
      val colRight = ('A'+(offsetPage+columnsPerCarril+offsetMiddle+offsetToIri)).toChar

      val pairsCompleted = ci.map(i => Some(i)).zipAll(cd.map(i => Some(i)),None,None)
      val n = pairsCompleted.length
      def convertOptionDR2SeqCells(optDR: Option[IriValueAfterProcess],indx: Int): Seq[Cell] = {
        def buildDesc(desc: String): String = {
          val (buffer, current) = desc.split(" ").foldLeft((List.empty[List[String]],List.empty[String])){case ((buffer,current),item) =>

            if((item :: current).mkString(" ").length >45){
              (current :: buffer,List(item))
            }else{
              (buffer, (item :: current))
            }

          }
          (current :: buffer).reverse.map(_.reverse.mkString(" ")).mkString("\n")
        }


        optDR.map{ e =>

          Seq(
            Cell(indx+1, style = CellStyle(borders = Reporter.bordersItem) ),
            Cell(e.progresiva,style = CellStyle(borders = Reporter.bordersItem,dataFormat = CellDataFormat("0+000"))),
            (e.iriValue.map(iri => Cell(iri)).getOrElse(Cell.Empty)).withStyle(CellStyle(dataFormat = CellDataFormat("0.00;-0.00;—;@"),borders=Reporter.bordersItem)),
            Cell(s"${e.fileTag}[${e.itemOnFile}]",style = CellStyle(borders = Reporter.bordersItem,
              wrapText = true,
              font = Font(height = new Height(7,HeightUnit.Point))

            )),
            (e.description.map(desc => Cell(buildDesc(desc))).getOrElse(Cell.Empty)).withStyle(CellStyle(borders=Reporter.bordersItem,wrapText = true,
              font = Font(height = new Height(7,HeightUnit.Point)))))


        }.getOrElse(Seq(Cell.Empty,Cell.Empty,Cell.Empty,Cell.Empty,Cell.Empty))
      }

      val rowsCarriles: Seq[Row] = pairsCompleted.zipWithIndex.map{ case((left,right),indx) =>
        Row().withCells(convertOptionDR2SeqCells(left,indx) ++ List(middleCell) ++ convertOptionDR2SeqCells(right,indx))
      }

      val rowDesde = offsetRows+2
      val rowHasta = rowDesde+n

      val resultLabelStyle = CellStyle(borders = Reporter.leftBorders,
        fillForegroundColor = color2,fillPattern = CellFill.Solid, horizontalAlignment = CellHorizontalAlignment.Right)
      val rowN = Row().withCells(
        Cell.Empty,
        Cell("n=", style = resultLabelStyle),
        Cell(s"=COUNT($colLeft$rowDesde:$colLeft$rowHasta)",
          style = CellStyle(borders = Reporter.rightBorders)),
        Cell.Empty,Cell.Empty,middleCell,
        Cell.Empty,Cell("n=",style =resultLabelStyle),Cell(s"=COUNT($colRight$rowDesde:$colRight$rowHasta)",style = CellStyle(borders = Reporter.rightBorders)),Cell.Empty
      )
      val st = CellStyle(dataFormat = CellDataFormat("0.00;-0.00;—;@"),borders = Reporter.rightBorders)
      val sizeProgHI = ci.flatMap(_.iriValue).length
      val sizeProgHD = cd.flatMap(_.iriValue).length
      def genCellWithFunction(validItems: Int, col: Char, threahs: Int, funcName: String): Cell = {
        (if(validItems>threahs) Cell(s"=$funcName($col$rowDesde:$col$rowHasta)") else Cell(0)).withStyle(st)
      }
      val textCellFormulaPromLeft = genCellWithFunction(sizeProgHI,colLeft,0,"AVERAGE")
      val textCellFormulaPromRight = genCellWithFunction(sizeProgHD,colRight,0,"AVERAGE")
      val rowProm = Row().withCells(
        Cell.Empty,Cell("IRI prom=",style = resultLabelStyle),textCellFormulaPromLeft,Cell.Empty,Cell.Empty,middleCell,
        Cell.Empty,Cell("IRI prom=",style = resultLabelStyle),textCellFormulaPromRight,Cell.Empty
      )
      val textCellFormulaSTDVLeft =  genCellWithFunction(sizeProgHI,colLeft,1,"STDEV")
      val textCellFormulaSTDVRight = genCellWithFunction(sizeProgHD,colRight,1,"STDEV")
      val rowSTDV = Row().withCells(
        Cell.Empty,Cell("DesvStd=",style = resultLabelStyle),textCellFormulaSTDVLeft,Cell.Empty,Cell.Empty,middleCell,
        Cell.Empty,Cell("DesvStd=",style = resultLabelStyle),textCellFormulaSTDVRight,Cell.Empty
      )

      val d = calcD((inclusive+exclusive)/2)
      val rowD = Row().withCells(
        Cell.Empty,Cell("D =",style = resultLabelStyle),Cell(d,style=CellStyle(borders = Reporter.rightBorders)),Cell.Empty,Cell.Empty,middleCell,
        Cell.Empty,Cell("D =",style = resultLabelStyle),Cell(d,style=CellStyle(borders = Reporter.rightBorders)),Cell.Empty
      )

      def genCellIric(col: Char): Cell = {
        Cell(s"=$col${rowHasta+1+2} + $col${rowHasta+1+3}*$col${rowHasta+1+4}",
          style = CellStyle(dataFormat = CellDataFormat("0.00;-0.00;—;@"),borders = Reporter.rightBorders))
      }
      val rowIRIC = Row().withCells(
        Cell.Empty,
        Cell("IRIc (izq)=",style = resultLabelStyle),
        genCellIric(colLeft),
        Cell.Empty,
        Cell.Empty,middleCell,
        Cell.Empty,
        Cell("IRIc (der)=",style = resultLabelStyle),
        genCellIric(colRight),
        Cell.Empty
      )
/*
      val formulaIRIc = ((sizeProgHI,sizeProgHD) match {
        case (a,b) if a>0 && b> 0 => Cell(s"=($colLeft${rowHasta+1+5}+$colRight${rowHasta+1+5})/2.0")
        case (a,b) if a>0 && b == 0 => Cell(s"=($colLeft${rowHasta+1+5})")
        case (a,b) if a==0 && b > 0 => Cell(s"=($colRight${rowHasta+1+5})")
        case _ => Cell(0, style = st)
      }).withStyle(st.copy(borders = Some(Reporter.rightBordersTHICK)))

*/
      val formulaIRIc = Cell(s"=AVERAGE($colLeft${rowHasta+1+5},$colRight${rowHasta+1+5})").withStyle(CellStyle(borders = Reporter.rightBordersTHICK))
      val rowPromedio = Row().withCells(Cell.Empty,Cell.Empty,Cell.Empty,Cell.Empty,
        Cell("IRIc=",style = resultLabelStyle.copy(borders = Some(Reporter.leftBordersTHICK))),
        Cell.Empty.withStyle(CellStyle(
          borders = CellBorders(topStyle = CellBorderStyle.Thick,bottomStyle = CellBorderStyle.Thick),
          fillForegroundColor = color2,
          fillPattern = CellFill.Solid
        )),
        formulaIRIc)

      val secctionStr = s"Del km ${Progresiva(inclusive).show(withSpaces = true,0)} al km ${Progresiva(exclusive).show(withSpaces = true,0)}"
      val sheet = Sheet(name = s"km${inclusive/1000}",
        rows = {
          buildHeaderRows(secctionStr,withRef=true) ++ internalHeader ++
          rowsCarriles ++ Seq(Row(cells = (1 to 5).map(_ => Cell.Empty) ++ List(middleCell)),rowN,rowProm,rowSTDV,rowD,rowIRIC,Row(),Row(),rowPromedio)}.toList,
        images=List(Image(CellRange(0 -> 0,0->11),pathHeader)),
        properties = SheetProperties(autoBreaks = true,forceFormulaRecalculation = true)
      ).withColumns((0 to 10).toList.map{
          case 0 => Column(width = new Width(4,measureUnit = WidthUnit.Character))
          case 6 => Column(width = new Width(4,measureUnit = WidthUnit.Character))
          case 1 => Column(width = new Width(8,measureUnit = WidthUnit.Character))
          case 7 => Column(width = new Width(8,measureUnit = WidthUnit.Character))
          case 2 => Column(width = new Width(5,measureUnit = WidthUnit.Character))
          case 8 => Column(width = new Width(5,measureUnit = WidthUnit.Character))
          case 3 => Column(width = new Width(6,measureUnit = WidthUnit.Character))
          case 9 => Column(width = new Width(6,measureUnit = WidthUnit.Character))
          case 4 => Column(width = new Width(25,measureUnit = WidthUnit.Character))
          case 10 => Column(width = new Width(25,measureUnit = WidthUnit.Character))
          case 5 => Column(width = new Width(1,measureUnit = WidthUnit.Character))
          case _ => Column(autoSized = true)
        }).withMergedRegions(
        CellRange(offsetRows -> (offsetRows), 0 -> 4),
          CellRange(offsetRows -> (offsetRows),6 -> 10)
          ,CellRange(1 -> 1, 0 ->10),
        CellRange(2 -> 2, 0 ->10),
        CellRange(3 -> 3, 0 ->1),
        CellRange(3 -> 3, 2 ->10),
        CellRange(4 -> 4, 0 ->1),
        CellRange(4 -> 4, 2 ->10),

        CellRange(5 -> 5, 0 ->1),
        CellRange(5 -> 5, 2 ->10),
        CellRange(6 -> 6, 0 ->1),
        CellRange(6 -> 6, 2 ->10),
        CellRange(7 -> 7, 0 ->1),
        CellRange(7 -> 7, 2 ->10),
      )
      def createResultCarril(col: Char): ResultCarril = {
        ResultCarril(s"$col${rowHasta+3}",s"$col${rowHasta+4}",s"$col${rowHasta+5}",s"$col${rowHasta+6}")
      }
      ResultsLabel(sheet,inclusive,exclusive,
        createResultCarril(colLeft),
        createResultCarril(colRight),s"G${rowHasta+9}")



    }
  }

  def buildWorkBook(): Option[Path] = {

    try {
      val path = Files.createTempFile("report", ".xlsx")

      val sheetsInterval = createSheetsByInterval()
      Workbook().withSheets(
        createResumenSheet(sheetsInterval) ++ List(buildResumenFilesUsed())++
        sheetsInterval.map(_.sheet)).saveAsXlsx(path.toAbsolutePath.toString)
      Some(path)
    }catch{
      case e: Exception => {
        println(e)
        None
      }
    }
  }

}
object Reporter {
  val bordersItem = CellBorders(
    bottomStyle = CellBorderStyle.Thin, bottomColor = Color.Black,
    leftStyle = CellBorderStyle.Thin, leftColor = Color.Black,
    rightStyle = CellBorderStyle.Thin, rightColor = Color.Black,
    topStyle = CellBorderStyle.Thin,topColor = Color.Black

  )
  val bordersItemTHICK = CellBorders(
    bottomStyle = CellBorderStyle.Thick, bottomColor = Color.Black,
    leftStyle = CellBorderStyle.Thick, leftColor = Color.Black,
    rightStyle = CellBorderStyle.Thick, rightColor = Color.Black,
    topStyle = CellBorderStyle.Thick,topColor = Color.Black

  )
  val leftBorders = CellBorders(
    bottomStyle = CellBorderStyle.Thin, bottomColor = Color.Black,
    leftStyle = CellBorderStyle.Thin, leftColor = Color.Black,
    topStyle = CellBorderStyle.Thin,topColor = Color.Black
  )
  val leftBordersTHICK = CellBorders(
    bottomStyle = CellBorderStyle.Thick, bottomColor = Color.Black,
    leftStyle = CellBorderStyle.Thick, leftColor = Color.Black,
    topStyle = CellBorderStyle.Thick,topColor = Color.Black
  )
  val rightBorders = CellBorders(
    bottomStyle = CellBorderStyle.Thin, bottomColor = Color.Black,
    rightStyle = CellBorderStyle.Thin, rightColor = Color.Black,
    topStyle = CellBorderStyle.Thin,topColor = Color.Black

  )
  val rightBordersTHICK = CellBorders(
    bottomStyle = CellBorderStyle.Thick, bottomColor = Color.Black,
    rightStyle = CellBorderStyle.Thick, rightColor = Color.Black,
    topStyle = CellBorderStyle.Thick,topColor = Color.Black

  )
  object MorningColor{
    val color1 = Color(159,241,252)
    val color2 = Color(248,241,156)
    val color3 = Color(198,255,174)
    val color4 = Color(223,217,217)
    val color5 = Color(255,255,255)
    val color6 = Color(229,240,241)
  }

}

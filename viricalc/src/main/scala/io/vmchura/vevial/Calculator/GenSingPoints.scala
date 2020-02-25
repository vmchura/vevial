package io.vmchura.vevial.Calculator


import java.nio.file.{Files, Path}

import com.norbitltd.spoiwo.model._
import com.norbitltd.spoiwo.natures.csv.Model2CsvConversions._
import com.norbitltd.spoiwo.model.{Cell, CellDataFormat, CellRange, CellStyle, Image, Row, Sheet, Workbook}
import io.vmchura.vevial.Calculator.models.{IriProgWithEvents, RelevamientoIRIProgresivas}
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI


class GenSingPoints(ejeVial: EfficientEjeProgresiva, files: Seq[(java.io.File,String)], progFrom: Progresiva, progTo: Progresiva) {

  val relevamientosIRI =  files.map{ case (file,tag) =>

        val relevamientoIRI = RelevamientoIRI(file, cf => IriProgWithEvents(cf))
        if(relevamientoIRI.elements.isEmpty)
          throw  new IllegalArgumentException("data is empty")
        else{
          new RelevamientoIRIProgresivas(tag,relevamientoIRI,ejeVial,progFrom,progTo)


        }

    }

  def genSingularityPoints(): Option[Path] = {
    /**
      *  si iri>= 8, evento por mala lectura +-30
      *  si dos eventos están juntos por 100m, to_do el rango +-20
      * caso contrario +-30
      */


    val lecturas8 = relevamientosIRI.flatMap{ _.elements.filter(_.iriElementData.iriValue.exists(_ >= 8)).map{ r =>
        Evento(r.progresiva,r.progresiva,30,"Error en lectura del equipo")
      }
    }

    val porEventos = relevamientosIRI.flatMap{ rel =>

      val elements = {
        val filtered = rel.elements.filter(_.iriElementData.event.contains("vent"))
        (if(rel.isForward) filtered else filtered.reverse).toList
      }
      if(elements.isEmpty){
        Nil
      }else {

        val eventos100 = elements.zip(elements.tail).filter {
          case (a, b) => b.progresiva - a.progresiva <= 100

        }.map {
          case (a, b) => Evento(a.progresiva, b.progresiva, 20,
            s"Punto de singularidad desde " +
              s"${Progresiva(a.progresiva).show(true, 4)} hasta " +
              s"${Progresiva(b.progresiva).show(true, 4)}")
        }

        val eventosSimples = elements.filterNot(r => eventos100.exists(e100 => e100.from <= r.progresiva && r.progresiva <= e100.to)).map { r =>
          Evento(r.progresiva, r.progresiva, 30, s"Punto de singularidad en ${Progresiva(r.progresiva).show(true, 4)} +/- 30")
        }

        eventos100 ++ eventosSimples
      }

    }

    val totalEventos = (lecturas8 ++ porEventos).sortBy(_.from)





    val rowsSelected: Seq[Row] = totalEventos.zipWithIndex.map { case (e,indx) =>
      val seqCells = Seq(Cell(indx+1),
        Cell(e.reason),
        Cell("p"),
        Cell(e.delta),
        Cell(e.from),
        Cell(e.to))

      Row().withCells(seqCells)

    }


    val sh = Sheet(name = "SP").withRows(
      List(Row().withCellValues("item","desc","proglat","dat0","dat1","dat2","dat3")) ++ rowsSelected)


    try {
      val path = Files.createTempFile("reportSingPoint", ".csv")

      Workbook().withSheets(sh).saveAsCsv(path.toAbsolutePath.toString)
      Some(path)
    }catch{
      case e: Exception => {
        println(e)
        None
      }
    }





  }

  case class Evento(from: Int, to: Int, delta: Int, reason: String)



}



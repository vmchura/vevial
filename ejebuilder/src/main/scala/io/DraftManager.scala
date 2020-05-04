package io

import java.io.File

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection, TPoint}
import models.{GeoLinkGraph, GeoNode, TLinkPoint}

import scala.xml.{Document, Elem, Node, NodeSeq}

object DraftManager {
    def loadDraft(root: Node): TLinkPoint = {
        val links = (root \\ "link").map(loadSimpleLink).toList
        assert(links.nonEmpty)
        assert(links.zip(links.tail).forall { case (a, b) => a.out ==? b.in })
        val nodesIn = links.map(_.in.point).map(t => new GeoNode(t))
        val nodesOut = nodesIn.tail ++ List(new GeoNode(links.last.out.point))

        val linksWithCommonNode = links.zip(nodesIn).zip(nodesOut).map{
            case ((link,i),j) =>
            new GeoLinkGraph(PointUnitaryVector(i,link.in.direction),PointUnitaryVector(j,link.out.direction))
        }
        linksWithCommonNode.zip(linksWithCommonNode.tail).foreach{ case (a,b) =>
          a.next = Some(b)
          b.prev = Some(a)

        }
        linksWithCommonNode.head

    }
    def generateFilesAsXML(files: Seq[File]): Node = {
        <files>
            {files.map(f => <file>{f.getAbsolutePath} </file>)}
        </files>
    }

    def loadFiles(node: Node): Seq[File] = {
        (node \\ "file").map(f => new java.io.File(f.text.trim))
    }


    def generateDraftAsXML(lp: TLinkPoint): scala.xml.Elem = {
        val lists = lp.untilEnd()
        <eje>{lists.map(linkAsXML)}</eje>
    }
    def loadSimpleLink(node: Node): TLinkPoint = {

        (for{
            inNode <- (node \\ "in").headOption
            outNode <- (node \\ "out").headOption
        }yield{
            val in = loadPointUnitaryVector(inNode)
            val out = loadPointUnitaryVector(outNode)
            new GeoLinkGraph(in,out)
        }).getOrElse(throw new IllegalArgumentException("does not represent a link"))
    }
    private def loadPointUnitaryVector(node: Node): PointUnitaryVector = {
        (for{
            pointNode <- (node \\ "point").headOption
            directionNode <- (node \\ "direction").headOption

        }yield{
            val point = loadPoint(pointNode)
            val direction = loadDirection(directionNode)
            PointUnitaryVector(point,direction)
        }).getOrElse(throw new IllegalArgumentException("node doesnt not represent a point vector"))

    }
    private def loadPoint(node: Node): TPoint = {
        (for{
          x <-  (node \\ "x").headOption
          y <-  (node \\ "y").headOption
          xv <- x.text.toDoubleOption
          yv <- y.text.toDoubleOption
        }yield{
            Point(xv,yv)
        }).getOrElse(throw new IllegalArgumentException("node doesnt not represent a point"))
    }
    private def loadDirection(node: Node): TDirection = {

        if((node \@ "isDefined").equals("true")){
            (for{
                dx <-  (node \\ "dx").headOption
                dy <-  (node \\ "dy").headOption
                dxv <- dx.text.toDoubleOption
                dyv <- dy.text.toDoubleOption
            }yield{
                TDirection(dxv,dyv)
            }).getOrElse(throw new IllegalArgumentException("node doesnt not represent a point"))
        }else{
            TDirection()
        }

    }
    def saveDraft(path: String)(lp: TLinkPoint): Boolean = {
        try {
            scala.xml.XML.save(path,generateDraftAsXML(lp))
            true
        }catch {
            case _: Throwable => false
        }
    }
    def saveProject(path: String)(lp: TLinkPoint, files: Seq[File]): Boolean = {
        val eje = generateDraftAsXML(lp)
        val filesXML = generateFilesAsXML(files)
        val project = <project>
            {eje}
            {filesXML}
        </project>
        try {
            scala.xml.XML.save(path,project)
            true
        }catch {
            case _: Throwable => false
        }
    }
    def loadProject(node: Elem): (TLinkPoint,Seq[File]) = {
        (for{
            ejeNode <- (node \\ "project" \\ "eje").headOption
            filesNode <- (node \\ "project" \\ "files").headOption
        }yield{
            (loadDraft(ejeNode),loadFiles(filesNode))
        }).getOrElse(throw new IllegalArgumentException("root doesnt contain project / eje and project / files"))
    }
    private def pointAsXML(tpoint: TPoint): scala.xml.Elem = {
        <point>
            <x>{f"${tpoint.x}%.8f"}</x>
            <y>{f"${tpoint.y}%.8f"}</y>
        </point>
    }
    private def directionAsXML(tdirection: TDirection): scala.xml.Elem = {
        <direction isDefined={s"${tdirection.isInstanceOf[Direction]}"}>
            {
            tdirection match {
                case _: AnyDirection => NodeSeq.Empty
                case d: Direction =>
                    <dx>{f"${d.dx}%.8f"}</dx>
                      <dy>{f"${d.dy}%.8f"}</dy>
            }
            }

        </direction>
    }

    final def linkAsXML(link: TLinkPoint): scala.xml.Elem = {
        <link>
            <in>
                {pointAsXML(link.in.point)}
                {directionAsXML(link.in.direction)}
            </in>
            <out>
                {pointAsXML(link.out.point)}
                {directionAsXML(link.out.direction)}
            </out>
        </link>
    }
}

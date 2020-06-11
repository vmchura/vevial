package io

import java.io.File

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.PointUnitaryVector
import models.{GeoLinkGraph, GeoNode, TLinkPoint}

import scala.xml.{Elem, Node}

object DraftManager {

    import ElementsAsXML._

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



    final def linkAsXML(link: TLinkPoint): scala.xml.Elem = {
        <link>
            <in>
                {savePointAsXML(link.in.point)}
                {saveDirectionAsXML(link.in.direction)}
            </in>
            <out>
                {savePointAsXML(link.out.point)}
                {saveDirectionAsXML(link.out.direction)}
            </out>
        </link>
    }
}

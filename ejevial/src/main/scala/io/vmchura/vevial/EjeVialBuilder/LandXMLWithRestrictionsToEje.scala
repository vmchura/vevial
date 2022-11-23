package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint

import java.io.InputStreamReader

class LandXMLWithRestrictionsToEje(source: InputStreamReader, restrictions: Iterable[ProgresivePoint]) extends LandXMLToEje(source) {
  override def getSequenceProgresivePoint: Iterable[ProgresivePoint] = restrictions

}

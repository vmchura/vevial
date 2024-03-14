package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.EjeVialUtil.Coordinates

object ExpectedPoints {
  val points: List[(Coordinates, Map[String, (Double, Double, String)])] = List(
    (Coordinates(latitud= -14.004850, longitud= -72.797108),
      Map(
        "PGV"-> (73900, 74000, "TRAMO_3S-F_UTM"),
        "ODNA"-> (73800, 73900, "Tramo I"))),
    (Coordinates(latitud = -13.689452, longitud = -72.913880),
      Map(
        "PGV" -> (100, 200, "TRAMO_3S-F_UTM"),
        "ODNA" -> (100, 200, "Tramo I"))),
//    (Coordinates(latitud = -14.103334, longitud = -72.706374),
//      Map(
//        "PGV" -> (101300, 101400, "TRAMO_3S-F_UTM"),
//        "ODNA" -> (101200, 101300, "Tramo I"))),
    (Coordinates(latitud = -14.103292, longitud = -72.708132),
      Map(
        "PGV" -> (101000, 101200, "TRAMO_3S-F_UTM"),
        "ODNA" -> (100900, 101000, "Tramo I"))),
    (Coordinates(latitud = -14.104633, longitud = -72.706526),
      Map(
        "PGV" -> (101700, 101800, "TRAMO_3S-F_UTM"),
        "ODNA" -> (101600, 101700, "Tramo II"))),
    (Coordinates(latitud = -14.060505, longitud = -72.568441),
      Map(
        "PGV" -> (145900, 146000, "TRAMO_3S-F_UTM"),
        "ODNA" -> (145700, 145800, "Tramo II"))),
    (Coordinates(latitud = -14.063603, longitud = -72.570276),
      Map(
        "PGV" -> (146600, 146700, "TRAMO_3S-F_UTM"),
        "ODNA" -> (146400, 146500, "Tramo IIIa"))),
    (Coordinates(latitud = -14.119593, longitud = -72.249428),
      Map(
        "PGV" -> (219400, 219500, "TRAMO_3S-F_UTM"),
        "ODNA" -> (21900, 219100, "Tramo IIIa"))),
    (Coordinates(latitud = -14.118153, longitud = -72.246017),
      Map(
        "PGV" -> (100, 200, "TRAMO_3S-G_UTM"),
        "ODNA" -> (100, 200, "Tramo IIIb"))),
    (Coordinates(latitud = -14.158251, longitud = -72.221156),
      Map(
        "PGV" -> (10200, 10300, "TRAMO_3S-G_UTM"),
        "ODNA" -> (10200, 10300, "Tramo IIIb"))),
    (Coordinates(latitud = -14.874001, longitud = -70.659430),
      Map(
        "PGV" -> (352800, 352900, "TRAMO_3S-G_UTM"),
        "ODNA" -> (352700, 352800, "Tramo IX"))),
  )
}

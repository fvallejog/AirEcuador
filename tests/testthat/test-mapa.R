test_that("the default Quito catalogue is preserved and city matching is explicit", {
  quito <- .estaciones_mapa()
  expect_equal(nrow(quito), 9L)
  expect_identical(quito, .estaciones_mapa(" Quito "))
  expect_identical(quito, .estaciones_mapa("QUITO"))
  expect_true(all(quito$ciudad == "Quito"))
  expect_true(all(abs(quito$latitud) < 1))
  expect_true("SAN_ANTONIO" %in% quito$codigo)
})

test_that("Cuenca retains UDA alongside the three separately identified EMOV sites", {
  cuenca <- .estaciones_mapa("cuenca")
  expect_setequal(cuenca$codigo, c("MUN", "CCA", "EIE", "UDA"))
  expect_equal(nrow(cuenca), 4L)
  emov <- cuenca[cuenca$codigo != "UDA", ]
  uda <- cuenca[cuenca$codigo == "UDA", ]
  expect_true(all(emov$red == "EMOV - CAIRE"))
  expect_true(all(emov$anio_coordenadas == 2013L))
  expect_true(all(!emov$descarga_implementada))
  expect_match(uda$red, "IERSE")
  expect_true(uda$descarga_implementada)
  expect_equal(uda$fuente_coordenadas, "https://ierse.uazuay.edu.ec/aire/")
  expect_true(all(nzchar(cuenca$fuente_coordenadas)))
  expect_true(all(cuenca$latitud > -3 & cuenca$latitud < -2.8))
  expect_true(all(cuenca$longitud > -79.1 & cuenca$longitud < -78.9))
  sitios <- cuenca[match(c("MUN", "CCA", "EIE"), cuenca$codigo), ]
  expect_gt(sitios$latitud[2L], sitios$latitud[1L])
  expect_gt(sitios$longitud[2L], sitios$longitud[1L])
  expect_lt(sitios$latitud[3L], sitios$latitud[1L])
  expect_lt(sitios$longitud[3L], sitios$longitud[1L])
})

test_that("invalid cities are rejected before requiring optional map software", {
  for (ciudad in list(NULL, NA_character_, 1, character(), "", "Ambato",
                      c("Quito", "Cuenca"))) {
    expect_error(mapa_estaciones(ciudad), "ciudad must be one of: Quito, Cuenca")
  }
})

test_that("both city selections can be rendered with leaflet", {
  skip_if_not_installed("leaflet")
  expect_s3_class(mapa_estaciones(), "leaflet")
  expect_s3_class(mapa_estaciones("Cuenca"), "leaflet")
})

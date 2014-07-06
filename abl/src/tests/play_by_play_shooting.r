source("src/shooting/shooting.r")

test.splitting.of.basket.hotel.coordinates <- function() {
  expect_that(splitCoordinates("175+37"), equals(c(175,37)))
  expect_that(splitCoordinates("125+23"), equals(c(125,23)))
}

test.splitting.of.basket.hotel.coordinates.x <- function() {
  expect_that(splitCoordinatesX("175+37"), equals(175))
}

test.splitting.of.basket.hotel.coordinates.y <- function() {
  expect_that(splitCoordinatesY("175+37"), equals(37))
}

test.shooting.zones <- function() {
  expect_that(getShootingZone(12,35), equals('left.corner.3'))
  expect_that(getShootingZone(30,135), equals('left.above.the.break.3'))
}
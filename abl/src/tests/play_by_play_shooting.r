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
  expect_that(getShootingZone(140,160), equals('top.3'))
  expect_that(getShootingZone(280-12,35), equals('right.corner.3'))
  expect_that(getShootingZone(280-30,135), equals('right.above.the.break.3'))
  expect_that(getShootingZone(127,25), equals('right.at.basket'))
  
}
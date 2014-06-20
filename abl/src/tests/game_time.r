source('src/AdvancedPlayerStats.R')

test.normal.40.minute.game <- function() {
  expect_that(GameTimeFromPlayerSummarizedMinutes(40 * 5), equals(40))
}

test.normal.40.minute.game.last3.minutes.with.4.players <- function() {
  checkEqualsNumeric(40.0, GameTimeFromPlayerSummarizedMinutes(40 * 5 - 3))
}

test.45.minute.game.last3.minutes.with.4.players <- function() {
  checkEqualsNumeric(45.0, GameTimeFromPlayerSummarizedMinutes(45 * 5 - 3))
}

test.single.OT.40.minute.game <- function() {
  checkEqualsNumeric(45.0, GameTimeFromPlayerSummarizedMinutes(45 * 5))
}
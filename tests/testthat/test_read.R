## Copyright 2017 Timothée Flutre, Université Paris-Diderot
##
## This file is part of UrbanTempo.
##
## UrbanTempo is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## UrbanTempo is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU Affero General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with UrbanTempo.  If not, see
## <http://www.gnu.org/licenses/>.

library(UrbanTempo)
context("Read")

test_that("readTemporalities_unsorted", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23", "2017-04-23"),
                              time=c("07:24", "07:13"),
                              space1=c("shop=end", "shop=start"),
                              space2=c("hawker", NA),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expected <- list(
      space1=data.frame(id=c("shop"),
                        start=c(strptime("2017-04-23 07:13",
                                         "%Y-%m-%d %H:%M")),
                        end=c(strptime("2017-04-23 07:24",
                                       "%Y-%m-%d %H:%M")),
                        stringsAsFactors=FALSE),
      space2=data.frame(id=c("hawker"),
                        start=c(strptime("2017-04-23 07:24",
                                         "%Y-%m-%d %H:%M")),
                        end=c(strptime("2017-04-23 07:25",
                                       "%Y-%m-%d %H:%M")),
                        stringsAsFactors=FALSE))

  observed <- readTemporalities(file=tmpf, duration.event=60, verbose=0)

  expect_equal(observed, expected)

  if(file.exists(tmpf))
    file.remove(tmpf)
})

test_that("readTemporalities_error_two_spaces", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23", "2017-04-23"),
                              time=c("07:24", "07:13"),
                              space1=c("shop=end", "shop=start X"),
                              space2=c("hawker", NA),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expect_error(readTemporalities(file=tmpf, verbose=0))

  if(file.exists(tmpf))
    file.remove(tmpf)
})

test_that("readTemporalities_error_not_finished_by_start_or_end", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23", "2017-04-23"),
                              time=c("07:24", "07:13"),
                              space1=c("shop=end", "shop=starting"),
                              space2=c("hawker", NA),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expect_error(readTemporalities(file=tmpf, verbose=0))

  if(file.exists(tmpf))
    file.remove(tmpf)
})

test_that("readTemporalities_error_end_without_start", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23"),
                              time=c("07:24"),
                              space1=c("shop=end"),
                              space2=c("hawker"),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expect_error(readTemporalities(file=tmpf, verbose=0))

  if(file.exists(tmpf))
    file.remove(tmpf)
})

test_that("readTemporalities_error_start_without_end", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23"),
                              time=c("07:13"),
                              space1=c("shop=start"),
                              space2=c("hawker"),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expect_error(readTemporalities(file=tmpf, verbose=0))

  if(file.exists(tmpf))
    file.remove(tmpf)
})

test_that("readTemporalities_mult-occurr-tempo-types-in-same-space-type", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23"),
                              time=c("07:13", "07:20",
                                     "07:33", "07:50"),
                              space1=c("shop=start", "shop=end",
                                       "shop=start", "shop=end"),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expected <- list(
      space1=data.frame(id=rep("shop", 2),
                        start=c(strptime("2017-04-23 07:13",
                                         "%Y-%m-%d %H:%M"),
                                strptime("2017-04-23 07:33",
                                         "%Y-%m-%d %H:%M")),
                        end=c(strptime("2017-04-23 07:20",
                                       "%Y-%m-%d %H:%M"),
                              strptime("2017-04-23 07:50",
                                       "%Y-%m-%d %H:%M")),
                        stringsAsFactors=FALSE))

  observed <- readTemporalities(file=tmpf, verbose=0)

  expect_equal(observed, expected)

  if(file.exists(tmpf))
    file.remove(tmpf)
})

test_that("readTemporalities_various_dates", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23", "2017-05-02"),
                              time=c("07:24", "07:13"),
                              space1=c("hawker", "collector"),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expected <- list(
      space1=data.frame(id=c("collector", "hawker"),
                        start=c(strptime(c("2017-05-02 07:13",
                                           "2017-04-23 07:24"),
                                         "%Y-%m-%d %H:%M")),
                        end=c(strptime(c("2017-05-02 07:14",
                                         "2017-04-23 07:25"),
                                       "%Y-%m-%d %H:%M")),
                        stringsAsFactors=FALSE))

  observed <- readTemporalities(file=tmpf, duration.event=60, verbose=0)

  expect_equal(observed, expected)

  if(file.exists(tmpf))
    file.remove(tmpf)
})

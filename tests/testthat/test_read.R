library(UrbanTempo)
context("Read")

test_that("readTemporalities_unsorted", {
  tmpd <- tempdir()

  temporalities <- data.frame(date=c("2017-04-23", "2017-04-23"),
                              time=c("07:24", "07:13"),
                              space1=c("shop end", "shop start"),
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
                              space1=c("shop end", "shop start X"),
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
                              space1=c("shop end", "shop starting"),
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
                              space1=c("shop end"),
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
                              space1=c("shop start"),
                              space2=c("hawker"),
                              stringsAsFactors=FALSE)
  tmpf <- paste0(tmpd, "/input_temporalities.tsv")
  write.table(x=temporalities, file=tmpf, quote=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE)

  expect_error(readTemporalities(file=tmpf, verbose=0))

  if(file.exists(tmpf))
    file.remove(tmpf)
})

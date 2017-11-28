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

##' Input of urban temporalities
##'
##' Reads a file containing urban temporalities in table format and returns a list of data frame(s) from it.
##' @param file the name of the file which the data are to be read from.
##' Space types are in columns and each row corresponds to a specific time.
##' The first line (header) should contain column names, and columns should be separated by a tabulation:
##' * first column: should be named `"date"` and formatted as year-month-day, for instance `"2017-04-23"` (a single file can contain data collected at different days);
##' * second column: should be named `"time"` and formatted as hour:minute, for instance `"17:02"`;
##' * third column: corresponds to the first space type;
##' * fourth column: corresponds to the second space type (if any);
##' * and so on, if there are several space types.
##' @param duration.event duration of a ponctual event, in seconds
##' @param verbose verbosity level (0/1/2)
##' @return list of data frame(s), one per space type
##' @seealso [readAppearances()], [plotTemporalities()]
##' @author Timothee Flutre
##' @examples
##' ## retrieve the path to the example data file provided with the package
##' f1 <- system.file("extdata", "Gibert_2014_data-PhD-thesis.tsv",
##'                   package="UrbanTempo")
##'
##' ## read the file
##' tpr <- readTemporalities(file=f1)
##'
##' ## for each space type, look at its temporality types
##' lapply(tpr, function(x){unique(x$id)})
##' @export
readTemporalities <- function(file, duration.event=60, verbose=1){
  stopifnot(is.character(file),
            is.numeric(duration.event),
            length(duration.event) == 1,
            is.numeric(verbose) || is.integer(verbose),
            length(verbose) == 1)

  out <- list()

  if(verbose > 0){
    msg <- "read the input file into a data frame, and check it..."
    message(msg)
  }
  input <- utils::read.table(file=file, header=TRUE, sep="\t", comment.char="#",
                             stringsAsFactors=FALSE)
  stopifnot(ncol(input) >= 3,
            all(colnames(input)[1:2] == c("date", "time")))

  if(verbose > 0){
    msg <- "sort it according to the 'time' column..."
    message(msg)
  }
  input$datetime <- as.POSIXlt(paste(input$date, input$time))
  input <- input[order(input$time),]

  if(verbose > 0){
    msg <- "reformat each column corresponding to a space type..."
    message(msg)
  }
  for(col.idx in 3:(ncol(input)-1)){ # for each space-type column
    space.type <- colnames(input)[col.idx]
    df <- data.frame(id=NA,
                     start=Sys.time(),
                     end=Sys.time() + 1,
                     stringsAsFactors=FALSE)
    df$start <- as.POSIXlt(df$start)
    df$end <- as.POSIXlt(df$end)

    for(i in 1:nrow(input)){ # for each row
      if(is.na(input[i, col.idx]))
        next
      input[i, col.idx] <- gsub(" ;", ";", input[i, col.idx])
      events <- strsplit(input[i, col.idx], ";")[[1]]
      events <- trimws(events)

      for(event in events){ # handle each event (not efficient code...)
        event <- strsplit(event, "=")[[1]]

        if(length(event) > 2){
          msg <- paste0("for space type '", space.type, "'",
                        " at time '", input$time[i], "'",
                        " (i=", i, "),",
                        " event '", paste(event, collapse=" "), "'",
                        " has more than one space")
          stop(msg)
        } else if(length(event) == 2){ # interval boundary (start or end)
          if(! event[2] %in% c("start", "end")){
            msg <- paste0("for space type '", space.type, "'",
                          " at time '", input$time[i], "'",
                          " (i=", i, "),",
                          " interval boundary '", paste(event, collapse=" "), "'",
                          " should finish by 'start' or 'end'")
            stop(msg)
          } else if(event[2] == "start"){
            df <- rbind(df,
                        data.frame(id=event[1],
                                   start=input$datetime[i],
                                   end=NA))
          } else if(event[2] == "end"){
            if(! event[1] %in% df$id){
              msg <- paste0("for space type '", space.type, "'",
                            " at time '", input$time[i], "'",
                            " (i=", i, "),",
                            " interval end '", paste(event, collapse=" "), "'",
                            " has no specified start")
              stop(msg)
            }
            idx <- which(df$id == event[1])
            if(length(idx) > 1)
              idx <- idx[length(idx)]
            df[idx,]$end <- input$datetime[i]
          }
        } else if(length(event) == 1){ # ponctual event
          df <- rbind(df,
                      data.frame(id=event,
                                 start=input$datetime[i],
                                 end=input$datetime[i] + duration.event))
        }
      }
    } # end of "for each row"

    df <- df[-1,]
    rownames(df) <- NULL
    idx <- which(is.na(df$end))
    if(length(idx) > 0){
      msg <- paste0("for space type '", space.type, "'",
                    " at start time '", df$start[idx[1]], "'",
                    " (i=", i, "),",
                    " interval '", df$id[idx[1]], "'",
                    " has no specified end")
      stop(msg)
    }

    out[[space.type]] <- df
  } # end of "for each space-type column"

  ## show a brief summary
  if(verbose > 0){
    msg <- paste0("summary: ", length(out), " space type",
                  ifelse(length(out) > 1, "s", ""))
    for(i in 1:length(out))
      msg <- paste0(msg, "\n (", i, ") ", names(out)[i], ": ",
                    length(unique(out[[i]]$id)), " temporality type",
                    ifelse(length(unique(out[[i]]$id)) > 1, "s", ""), ", ",
                    nrow(out[[i]]), " time point",
                    ifelse(nrow(out[[i]]) > 1, "s", ""))
    message(msg)
  }

  return(out)
}

##' Input of appearances
##'
##' Reads a file containing the appearances in table format, and returns a list of data frame(s) from it.
##' @param file the name of the file which the data are to be read from.
##' The content of this file should correspond to urban temporalities as read by [readTemporalities()], with one row per event.
##' The first line (header) should contain column names, and columns should be separated by a tabulation:
##' * first column: should be named `"space"`;
##' * second column: should be named `"event"`;
##' * third column: should be named `"color"` (choose among all possibilities as explained \href{http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf}{here} and \href{https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf}{there});
##' * (optional) fourth column: should be named `"density"`.
##' @param verbose verbosity level (0/1/2)
##' @return list of data frame(s), one per space type
##' @seealso [readTemporalities()], [plotTemporalities()]
##' @author Timothee Flutre
##' @examples
##' ## retrieve the path to the example color file provided with the package
##' f2 <- system.file("extdata", "Gibert_2014_colors-PhD-thesis.tsv",
##'                   package="UrbanTempo")
##'
##' ## read the file
##' app <- readAppearances(file=f2)
##' @export
readAppearances <- function(file, verbose=1){
  stopifnot(is.character(file),
            is.numeric(verbose) || is.integer(verbose),
            length(verbose) == 1)

  out <- list()

  if(verbose > 0){
    msg <- "read the input file into a data frame, and check it..."
    message(msg)
  }
  input <- utils::read.table(file=file, header=TRUE, sep="\t", comment.char="#",
                             stringsAsFactors=FALSE)
  stopifnot(ncol(input) >= 3,
            all(c("space", "event", "color") %in% colnames(input)))

  idx.empty <- which(input$color == "")
  if(length(idx.empty) > 0){
    msg <- paste0("replace missing color for event '", input$event[idx.empty[1]], "'",
                  " in space type '", input$space[idx.empty[1]], "' by 'black'")
    warning(msg)
    input$color[idx.empty] <- "black"
  }
  if("density" %in% colnames(input)){
    idx.empty <- which(input$density == "")
    if(length(idx.empty) > 0)
      input$density[idx.empty] <- NA
  }

  space.types <- unique(input$space)
  for(space.type in space.types){
    idx <- which(input$space == space.type)
    out[[space.type]] <- data.frame(color=input[idx, "color"],
                                    stringsAsFactors=FALSE)
    if(anyDuplicated(input[idx, "event"])){
      dupl.event <- input[idx, "event"][duplicated(input[idx, "event"])][1]
      msg <- paste0("for space type '", space.type, "',",
                    " event '", dupl.event, "' is duplicated")
      stop(msg)
    }
    rownames(out[[space.type]]) <- input[idx, "event"]
    if("density" %in% colnames(input))
      out[[space.type]]$density <- input[idx, "density"]
  }

  ## show a brief summary
  if(verbose > 0){
    msg <- paste0("summary: ", length(out), " space type",
                  ifelse(length(out) > 1, "s", ""))
    for(i in 1:length(out))
      msg <- paste0(msg, "\n (", i, ") ", names(out)[i], ": ",
                    length(rownames(out[[i]])), " temporality type",
                    ifelse(length(rownames(out[[i]])) > 1, "s", ""))
    message(msg)
  }

  return(out)
}

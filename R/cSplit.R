#' Split Concatenated Values into Separate Values
#' 
#' The \code{cSplit} function is designed to quickly and conveniently split
#' concatenated data into separate values.
#' 
#' 
#' @param indt The input \code{data.frame} or \code{data.table}.
#' @param splitCols The column or columns that need to be split.
#' @param sep The values that serve as a delimiter \emph{within} each column.
#' This can be a single value if all columns have the same delimiter, or a
#' vector of values \emph{in the same order as the delimiters in each of the
#' \code{splitCols}}.
#' @param direction The desired direction of the results, either \code{"wide"}
#' or \code{"long"}.
#' @param fixed Logical. Should the split character be treated as a fixed
#' pattern (\code{TRUE}) or a regular expression (\code{FALSE})? Defaults to
#' \code{TRUE}.
#' @param drop Logical. Should the original concatenated column be dropped?
#' Defaults to \code{TRUE}.
#' @param stripWhite Logical. If there is whitespace around the delimiter in
#' the concatenated columns, should it be stripped prior to splitting? Defaults
#' to \code{TRUE}.
#' @param makeEqual Logical. Should all groups be made to be the same length?
#' Defaults to \code{TRUE}.
#' @param type.convert Logical. Should \code{\link{type.convert}} be used to convert
#' the result of each column? This would add a little to the execution time. 
#' Defaults to \code{TRUE}.
#' @return A \code{\link[data.table:data.table]{data.table}} with the values
#' split into new columns or rows.
#' @note The \code{cSplit} function replaces most of the earlier
#' \code{concat.split*} functions. The earlier functions remain for
#' compatability purposes, but now they are essentially wrappers for the
#' \code{cSplit} function.
#' 
#' If you know that all values in the column would have the same number of values per row after being split, you should use the \code{\link{cSplit_f}} function instead, which uses \code{\link[data.table:fread]{fread}} instead of \code{\link{strsplit}} and is generally faster.
#' 
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{cSplit_f}}
#' @examples
#' 
#' ## Sample data
#' temp <- head(concat.test)
#' 
#' ## Split the "Likes" column
#' cSplit(temp, "Likes")
#' 
#' ## Split the "Likes" and "Hates" columns --
#' ##   they have different delimiters...
#' cSplit(temp, c("Likes", "Hates"), c(",", ";"))
#' 
#' ## Split "Siblings" into a long form...
#' cSplit(temp, "Siblings", ",", direction = "long")
#' 
#' ## Split a vector
#' y <- c("a_b_c", "a_b", "c_a_b")
#' cSplit(as.data.table(y), "y", "_")
#' 
#' @export cSplit
cSplit <- function(indt, splitCols, sep = ",", direction = "wide", 
                   fixed = TRUE, drop = TRUE, stripWhite = TRUE, 
                   type.convert = TRUE, makeEqual = TRUE) {
  
  if (!is.data.table(indt)) 
    indt <- as.data.table(indt)
  else indt <- copy(indt)
  if (is.numeric(splitCols)) 
    splitCols <- Names(indt, splitCols)
  
  if (direction == "long" & length(splitCols) > 1) {
    if (!isTRUE(makeEqual)) {
      message("makeEqual specified as FALSE but set to TRUE")
      makeEqual <- TRUE
    }
  }
  
  if (length(sep) == 1) 
    sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) {
    stop("Verify you have entered the correct number of sep")
  }
  
  switch(
    direction,
    wide = {
      X <- lapply(seq_along(splitCols), function(x) {
        temp1 <- stri_split_fixed(indt[[splitCols[x]]], sep[x], 
                                  simplify = TRUE, omit_empty = TRUE)
        if (isTRUE(stripWhite)) temp1 <- Trim(temp1)
        temp1 <- as.data.table(temp1)
        setnames(temp1, paste(splitCols[x], .pad(
          sequence(ncol(temp1))), sep = "_"))
        if (isTRUE(type.convert)) temp1 <- temp1[, lapply(.SD, type.convert)]
        temp1
      })
      out <- cbind(indt, do.call(cbind, X))
      if (isTRUE(drop)) out[, (splitCols) := NULL][]
    },
    long = {
      Y <- lapply(seq_along(splitCols), function(x) {
        temp1 <- stri_split_fixed(indt[[splitCols[x]]], sep[x],
                                  simplify = TRUE, omit_empty = TRUE)
      })
      Ncols <- max(vapply(Y, ncol, 1L))
      Y <- lapply(Y, function(x) {
        out <- c(t(padNAcols(x, Ncols)))
        if (isTRUE(stripWhite)) out <- stri_trim_both(out)
        out
      })
      YDT <- as.data.table(Y)
      setnames(YDT, paste0(splitCols, "_new"))
      if (isTRUE(type.convert)) YDT <- YDT[, lapply(.SD, type.convert)]
      out <- cbind(expandRows(indt, Ncols, count.is.col = FALSE), YDT)
      if (!isTRUE(makeEqual)) out <- na.omit(out, by = paste0(splitCols, "_new"))
      if (isTRUE(drop)) out[, (splitCols) := NULL][]
    })
}
NULL

#' @export
column_xs <- function(..., .width, .offset = 0) {

  if (.offset > 0) {

    offset_class <- str_c(
      " offset-xs-",
      .offset
    )

  } else {

    offset_class <- NULL

  }

  col_class <- str_c(
    "col-xs-",
    .width,
    offset_class
  )

  div(class = col_class, ...)

}

#' @export
column_sm <- function(..., .width, .offset = 0) {

  if (.offset > 0) {

    offset_class <- str_c(
      " offset-sm-",
      .offset
    )

  } else {

    offset_class <- NULL

  }

  col_class <- str_c(
    "col-sm-",
    .width,
    offset_class
  )

  div(class = col_class, ...)

}

#' @export
column_md <- function(..., .width, .offset = 0) {

  if (.offset > 0) {

    offset_class <- str_c(
      " offset-md-",
      .offset
    )

  } else {

    offset_class <- NULL

  }

  col_class <- str_c(
    "col-md-",
    .width,
    offset_class
  )

  div(class = col_class, ...)

}

#' @export
column_lg <- function(..., .width, .offset = 0) {

  if (.offset > 0) {

    offset_class <- str_c(
      " offset-lg-",
      .offset
    )

  } else {

    offset_class <- NULL

  }

  col_class <- str_c(
    "col-lg-",
    .width,
    offset_class
  )

  div(class = col_class, ...)

}

#' @export
column_xl <- function(..., .width, .offset = 0) {

  if (.offset > 0) {

    offset_class <- str_c(
      " offset-xl-",
      .offset
    )

  } else {

    offset_class <- NULL

  }

  col_class <- str_c(
    "col-xl-",
    .width,
    offset_class
  )

  div(class = col_class, ...)

}

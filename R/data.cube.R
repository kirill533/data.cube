data.cube <- function(dplyr_obj, json_schema = NULL) {

  a = list(
    db = dplyr_obj,
    schema = jsonlite::fromJSON(readChar(json_schema, file.info(json_schema)$size))
  )

  class(a) <- c('data.cube')

  a
}

#' @export
select.data.cube <- function(.data, rows = list(), columns = list(), ...) {

  ddb = .data$db
  schema = .data$schema

  tbl = NULL

  if (length(rows)) {
    for (i in rows) {
      tbl <- joinAttribute(tbl, rows[i], schema)
    }
  }

  NULL
}

joinAttribute <- function(tbl, attr, schema) {

}


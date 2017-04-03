# -*- encoding=utf -*-

# """
# cubes.sql.query
# ~~~~~~~~~~~~~~~
#
# Star/snowflake schema query construction structures
#
# """

# Note for developers and maintainers
# -----------------------------------
#
# This module is to be remained implemented in a way that it does not use any
# of the Cubes objects. It might use duck-typing and assume objects with
# similar attributes. No calls to Cubes object functions should be allowed
# here.

# Default label for all fact keys
FACT_KEY_LABEL = '__fact_key__'

# Attribute -> Column
# IF attribute has no 'expression' then mapping is used
# IF attribute has expression, the expression is used and underlying mappings

# Physical column (or column expression) reference. `schema` is a database
# schema name, `table` is a table (or table expression) name containing the
# `column`. `extract` is an element to be extracted from complex data type such
# as date or JSON (in postgres). `function` is name of unary function to be
# applied on the `column`.
#
# Note that either `extract` or `function` can be used, not both.

Column = setRefClass(
  "Column",
  fields = c("schema", "table", "column",
             "extract", "FUN")
)


#
# IMPORTANT: If you decide to extend the above Mapping functionality by adding
# other mapping attributes (not recommended, but still) or by changing the way
# how existing attributes are used, make sure that there are NO OTHER COLUMNS
# than the `column` used. Every column used MUST be accounted in the
# relevant_joins() call.
#
# See similar comment in the column() method of the StarSchema.
#

to_column = function(obj, default_table=NULL, default_schema=NULL) {
  # Utility function that will create a `Column` reference object from an
  # anonymous tuple, dictionary or a similar object. `obj` can also be a
  # string in form ``schema.table.column`` where shcema or both schema and
  # table can be ommited. `default_table` and `default_schema` are used when
  # no table or schema is provided in `obj`

  if (is.null(obj)) {
    stop('ArgumentError: Mapping object can not be None')
  }

  if (is.character(obj)) {
    obj = unlist(stringi::stri_split_fixed(obj, "."))
  }

  if (is.null(names(obj))) {

    column = NULL
    table = NULL
    schema = NULL
    if (length(obj) == 1) {
      column = obj
      table = NULL
      schema = NULL
    } else if (length(obj) == 2) {
      column = obj[2]
      table = obj[1]
      schema = NULL
    } else if (length(obj) == 3) {
      column = obj[3]
      table = obj[2]
      schema = obj[1]
    } else {
      stop(sprintf("Join key can have 1 to 3 items has %s", length(obj)))
    }
    extract = NULL
    FUN = NULL
  } else if (is.data.frame(obj) || is.list(obj)) {
      column = obj$column
      table = obj$table
      schema = obj$schema
      extract = obj$extract
      FUN = obj$FUN
  } else {
    stop('Unsupported format of obj provided to to_column function.')
  }

  if (is.null(table))
    table = default_table

  if (is.null(schema))
    schema = default_schema

  Column(schema=schema, table=table, column=column, extract=extract, FUN=FUN)
}




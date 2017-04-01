#' @include metadata_base.R


create_list_of <- function(class_, objects) {
  # Return a list of model objects of class `class_` from list of object
  #   metadata `objects`
  lapply(objects, function(obj) {do.call(paste0(class_$className, '.from_metadata'), list(metadata=obj))})
}


# Base class for dimension attributes, measures and measure
#   aggregates.
#
#   Attributes:
#
#   * `name` - attribute name, used as identifier
#   * `label` - attribute label displayed to a user
#   * `order` - default order of this attribute. If not specified, then
#     order is unexpected. Possible values are: ``'asc'`` or ``'desc'``.
#     It is recommended and safe to use ``Attribute.ASC`` and
#     ``Attribute.DESC``
#   * `info` - custom information dictionary, might be used to store
#     application/front-end specific information
#   * `format` - application-specific display format information, useful
#     for formatting numeric values of measure attributes
#   * `missing_value` – value to be used when there is no value (``NULL``)
#     in the data source. Support of this attribute property depends on the
#     backend. Please consult the backend documentation for more
#     information.
#   * `expression` – arithmetic expression for computing this attribute
#     from other existing attributes.
#
#   String representation of the `AttributeBase` returns its `name`.
#
#   `cubes.ArgumentError` is raised when unknown ordering type is
#   specified.
AttributeBase = setRefClass(
  'AttributeBase',
  contains = 'ModelObject',
  fields = c('order', 'format', 'expression', 'missing_value', 'ref', 'dimension'),
  methods = list(

    initialize = function(order=NULL, format=NULL, missing_value=NULL, expression=NULL, ...) {

      callSuper(...)

      format <<- format
      missing_value <<- missing_value
      # TODO: temporarily preserved, this should be present only in
      # Attribute object, not all kinds of attributes
      dimension <<- NULL

      expression <<- expression
      ref <<- name

      if (!is.null(order)) {
        order_lovercase <- stringr::str_to_lower(order)

        if (substr(order_lovercase, 1, 3) == 'asc') {
          order <<- Attribute.ASC
        } else if (substr(order_lovercase, 1, 3) == 'desc') {
          order <<- Attribute.DESC
        } else {
          stop(sprintf("ArgumentError: Unknown ordering '%s' for attributes '%s'", order, ref))
        }

      } else {
        order <<- NULL
      }

    }
  )
)


AttributeBase.from_metadata <- function(metadata=list(), class_ = AttributeBase) {
  if (is.character(metadata)) {
    do.call(class_$new, list(name=metadata))
  } else if (class_$className %in% class(metadata)) {
    metadata$copy()
  } else {
    if (is.null(metadata$name)) {
      stop("ModelError: Model objects metadata require at least name to be present.")
    }
    do.call(class_$new, metadata)
  }
}

# Dimension attribute object. Also used as fact detail.
#
# Attributes:
#
# * `name` - attribute name, used as identifier
# * `label` - attribute label displayed to a user
# * `locales` = list of locales that the attribute is localized to
# * `order` - default order of this attribute. If not specified, then
#   order is unexpected. Possible values are: ``'asc'`` or ``'desc'``.
#   It is recommended and safe to use ``Attribute.ASC`` and
#   ``Attribute.DESC``
# * `info` - custom information dictionary, might be used to store
#   application/front-end specific information
# * `format` - application-specific display format information, useful
#   for formatting numeric values of measure attributes
#
# String representation of the `Attribute` returns its `name` (without
# dimension prefix).
#
# `cubes.ArgumentError` is raised when unknown ordering type is
# specified.
#
# Note: copied attributes are dis-owned from dimension. The new
# dimension has to be assigned after copying.
Attribute = setRefClass('Attribute', contains = 'AttributeBase')



Attribute.from_metadata <- function(metadata=list()) {
  AttributeBase.from_metadata(metadata = metadata, class_ = Attribute)
}

# Create a measure attribute. Properties in addition to the attribute
# base properties:
#
# * `formula` – name of a formula for the measure
# * `aggregates` – list of default (relevant) aggregate functions that
#   can be applied to this measure attribute.
# * `nonadditive` – kind of non-additivity of the dimension. Possible
#   values: `NULL` (fully additive, default), ``time`` (non-additive for
#   time dimensions) or ``all`` (non-additive for any other dimension)
#
# Note that if the `formula` is specified, it should not refer to any
# other measure that refers to this one (no circular reference).
#
# The `aggregates` is an optional property and is used for:
# * measure aggergate object preparation
# * optional validation
#
# String representation of a `Measure` returns its full reference.
Measure = setRefClass(
  'Measure',
  contains = 'ModelObject',
  fields = c('aggregates', 'formula', 'nonadditive', 'window_size'),

  methods = list(

    initialize = function( aggregates=NULL, formula=NULL, nonadditive=NULL,
                           window_size=NULL, ...) {

      callSuper(...)

      formula <<- formula
      aggregates <<- aggregates
      window_size <<- window_size

      # Note: synchronize with Dimension.__init__ if relevant/necessary
      if (is.null(nonadditive) || nonadditive == "none") {
        nonadditive <<- NULL
      } else if (nonadditive %in% c("all", "any")) {
        nonadditive <<- "any"
      } else if (nonadditive == "time") {
        nonadditive <<- "time"
      } else {
        stop(sprintf("ModelError: Unknown non-additive measure type '%s'", nonadditive))
      }
    }
  )
)


Measure.from_metadata <- function(metadata=list()) {
  AttributeBase.from_metadata(metadata = metadata, class_ = Measure)
}


# Masure aggregate
#
# Attributes:
#
# * `function` – aggregation function for the measure
# * `formula` – name of a formula that contains the arithemtic
#   expression (optional)
# * `measure` – measure name for this aggregate (optional)
# * `expression` – arithmetic expression (only if backend supported)
# * `nonadditive` – additive behavior for the aggregate (inherited from
#   the measure in most of the times)
MeasureAggregate = setRefClass(
  'MeasureAggregate',
  contains = 'AttributeBase'
)


MeasureAggregate.from_metadata <- function(metadata=list()) {
  AttributeBase.from_metadata(metadata = metadata, class_ = MeasureAggregate)
}

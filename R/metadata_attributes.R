create_list_of <- function(class_, objects) {
  # Return a list of model objects of class `class_` from list of object
  #   metadata `objects`
  lapply(objects, function(obj) {do.call(paste0(AttributeBase$className, '.from_metadata'), obj)})
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
AttributeBase = setRefClass('AttributeBase', contains = 'ModelObject')


AttributeBase.from_metadata <- function() {

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


Attribute.from_metadata <- function() {
  Attribute$new()
}

# Create a measure attribute. Properties in addition to the attribute
# base properties:
#
# * `formula` – name of a formula for the measure
# * `aggregates` – list of default (relevant) aggregate functions that
#   can be applied to this measure attribute.
# * `nonadditive` – kind of non-additivity of the dimension. Possible
#   values: `none` (fully additive, default), ``time`` (non-additive for
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
  contains = 'AttributeBase'
)


Measure.from_metadata <- function() {
  Measure$new()
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
  'Measure',
  contains = 'AttributeBase'
)


MeasureAggregate.from_metadata <- function() {
  MeasureAggregate$new()
}

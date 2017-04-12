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

    initialize = function(name, order=NULL, format=NULL, missing_value=NULL, expression=NULL, ...) {

      callSuper(name = name, ...)

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

    },

    equal_to = function(other) {
      if (class(other)[1] != class(.self)[1]) {
        return(FALSE)
      }

      # TODO: should we be this strict?
      return(nvl(name, '') == nvl(other$name, '') &&
            nvl(label, '') == nvl(other$label, '') &&
            nvl(unlist(info), '') == nvl(unlist(other$info), '') &&
            nvl(description, '') == nvl(other$description, '') &&
            nvl(format, '') == nvl(other$format, '') &&
            nvl(expression, '') == nvl(other$expression, '') &&
            nvl(missing_value, '') == nvl(other$missing_value, ''))
    },

    to_dict = function(options) {
      d = callSuper(options)

      d$format = format
      d$order = order
      d$missing_value = missing_value
      d$expression = expression

      d$ref = ref

      d
    },

    copy = function(shallow = FALSE) {
      def <- .refClassDef
      value <- new(def, name = name)
      vEnv <- as.environment(value)
      selfEnv <- as.environment(.self)
      for (field in names(def@fieldClasses)) {
        if (field == 'dimension') {
          # do nothing
        } else if (shallow) {
          assign(field, get(field, envir = selfEnv), envir = vEnv)
        } else {
          current <- get(field, envir = selfEnv)
          if (is(current, "envRefClass")) {
            current <- current$copy(FALSE)
          }
          assign(field, current, envir = vEnv)
        }
      }
      value
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

as.character.AttributeBase = function(attr) {
  attr$ref
}

as.vector.AttributeBase = function(attr, mode = "any") {
  attr$ref
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
Attribute = setRefClass(
  'Attribute',
  contains = 'AttributeBase',

  fields = c('dimension'),
  methods = list(

    initialize = function(name, label=NULL, description=NULL, order=NULL,
                          info=NULL, format=NULL, dimension=NULL,
                          missing_value=NULL, expression=NULL, ...) {

      # """Dimension attribute object. Also used as fact detail.
      #
      # Attributes:
      #
      # * `name` - attribute name, used as identifier
      # * `label` - attribute label displayed to a user
      # * `locales` = list of locales that the attribute is localized to
      # * `order` - default order of this attribute. If not specified, then
      # order is unexpected. Possible values are: ``'asc'`` or ``'desc'``.
      # It is recommended and safe to use ``Attribute.ASC`` and
      # ``Attribute.DESC``
      # * `info` - custom information dictionary, might be used to store
      # application/front-end specific information
      # * `format` - application-specific display format information, useful
      # for formatting numeric values of measure attributes
      #
      # String representation of the `Attribute` returns its `name` (without
      # dimension prefix).
      #
      # `cubes.ArgumentError` is raised when unknown ordering type is
      # specified.
      #
      # Note: copied attributes are dis-owned from dimension. The new
      # dimension has to be assigned after copying.
      # """

      callSuper(name=name, label=label,
                description=description, order=order,
                info=info, format=format,
                missing_value=missing_value,
                expression=expression, ...)


      dimension <<- NULL

      setDimension(dimension)

    },

    setDimension = function(dimension) {

      if (!is.null(dimension)) {

        if (dimension$is_flat() && !dimension$has_details()) {
          ref <<- dimension$name
        } else {
          ref <<- paste0(dimension$name, '.', name)
        }
      } else {
        ref <<- name
      }
      dimension <<- dimension
    },

    getDimension = function() {
      dimension
    },

    to_dict = function(options) {
      # FIXME: Depreciated key "full_name" in favour of "ref"
      d = callSuper(options)

      d
    }
  )

)



Attribute.from_metadata <- function(metadata=list()) {
  # do not support locales
  if (is.list(metadata) && !is.null(metadata$locales))
    metadata$locales = NULL

  AttributeBase.from_metadata(metadata = metadata, class_ = Attribute)
}

as.character.Attribute = function(attr) {
  attr$ref
}

as.vector.Attribute = function(attr, mode = "any") {
  attr$ref
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
  contains = 'AttributeBase',
  fields = c('aggregates', 'formula', 'nonadditive', 'window_size'),

  methods = list(

    initialize = function(name, aggregates=NULL, formula=NULL, nonadditive=NULL,
                           window_size=NULL, ...) {

      callSuper(name=name, ...)

      formula <<- formula
      aggregates <<- aggregates
      window_size <<- window_size

      # Note: synchronize with Dimension.__init__ if relevant/necessary
      if (is.null(nonadditive) || nonadditive == "None") {
        nonadditive <<- NULL
      } else if (nonadditive %in% c("all", "any")) {
        nonadditive <<- "any"
      } else if (nonadditive == "time") {
        nonadditive <<- "time"
      } else {
        stop(sprintf("ModelError: Unknown non-additive measure type '%s'", nonadditive))
      }
    },

    to_dict = function(options) {
      d = callSuper(options)
      d$formula = formula
      d$aggregates = aggregates
      d$window_size = window_size

      d
    },

    default_aggregates = function() {
      # """Creates default measure aggregates from a list of receiver's
      #   measures. This is just a convenience function, correct models should
      #   contain explicit list of aggregates. If no aggregates are specified,
      #   then the only aggregate `sum` is assumed.
      #   """

      loc_aggregates = list()

      for (agg in nvl(aggregates, 'sum')) {
        if (agg == "identity") {
          loc_name = .self$name
          measure = NULL
          FUN = NULL
        } else {
          loc_name = paste0(.self$name, '_', agg)
          measure = .self$name
          FUN = agg
        }

        aggregate = MeasureAggregate(name=loc_name,
                                     label=NULL,
                                     description=description,
                                     order=order,
                                     info=info,
                                     format=format,
                                     measure=measure,
                                     FUN=FUN,
                                     window_size=window_size)

        loc_aggregates[[length(loc_aggregates) + 1]] = aggregate
      }

      loc_aggregates
    }
  )
)


Measure.from_metadata <- function(metadata=list()) {
  AttributeBase.from_metadata(metadata = metadata, class_ = Measure)
}


as.character.Measure = function(attr) {
  attr$ref
}

as.vector.Measure = function(attr, mode = "any") {
  attr$ref
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
  contains = 'AttributeBase',
  fields = c('FUN', 'formula', 'measure', 'nonadditive', 'window_size'),
  methods = list(
    initialize = function(self, name, label=NULL, description=NULL, order=NULL,
                 info=NULL, format=NULL, missing_value=NULL, measure=NULL,
                 FUN=NULL, formula=NULL, expression=NULL,
                 nonadditive=NULL, window_size=NULL, ...) {
      # """Masure aggregate
      #
      #   Attributes:
      #
      #   * `function` – aggregation function for the measure
      #   * `formula` – name of a formula that contains the arithemtic
      #     expression (optional)
      #   * `measure` – measure name for this aggregate (optional)
      #   * `expression` – arithmetic expression (only if backend supported)
      #   * `nonadditive` – additive behavior for the aggregate (inherited from
      #     the measure in most of the times)
      #   """

      callSuper(name=name, label=label,
               description=description,
               order=order, info=info,
               format=format,
               missing_value=missing_value,
               expression=expression)

      FUN <<- FUN
      formula <<- formula
      measure <<- measure
      nonadditive <<- nonadditive
      window_size <<- window_size

    },

    to_dict = function(options) {
      d = callSuper(options)
      d$FUN = FUN
      d$formula = formula
      d$measure = measure
      d$nonadditive = nonadditive
      d$window_size = window_size

      d
    }

  )
)


MeasureAggregate.from_metadata <- function(metadata=list()) {

  if (!is.character(metadata) && is.null(metadata$FUN) && !is.null(metadata[['function']])) {
    metadata$FUN = metadata[['function']]
    metadata[['function']] <- NULL
  }

  AttributeBase.from_metadata(metadata = metadata, class_ = MeasureAggregate)
}

as.character.MeasureAggregate = function(attr) {
  attr$ref
}

as.vector.MeasureAggregate = function(attr, mode = "any") {
  attr$ref
}

DEFAULT_FACT_COUNT_AGGREGATE = list(
  "name" = "fact_count",
  "label" = "Count",
  "FUN" = "count"
)

IMPLICIT_AGGREGATE_LABELS = list(
  "sum"= "Sum of %s",
  "count"= "Record Count",
  "count_nonempty"= "Non-empty count of %s",
  "min"= "%s Minimum",
  "max"= "%s Maximum",
  "avg"= "Average of %s"
)


expand_attribute_metadata <- function(metadata) {
  # Fixes metadata of an attribute. If `metadata` is a string it will be
  #   converted into a dictionary with key `"name"` set to the string value.

  if (is.character(metadata)) {
    metadata = list("name"= metadata)
  }

  metadata
}


Cube = setRefClass(
  'Cube',

  contains = 'ModelObject',

  fields = c('name', 'pvt_dimensions', 'pvt_measures', 'pvt_aggregates',
             'label', 'pvt_details', 'mappings', 'joins',
             'fact', 'key', 'description', 'browser_options',
             'info', 'dimension_links', 'locale', 'category',
             'store', 'browser', 'store_name', 'basename'),

  methods = list(

    initialize = function(name = NULL, dimensions = NULL, measures = NULL, aggregates = NULL,
                          label = NULL, details = NULL, mappings = NULL, joins = NULL,
                          fact = NULL, key = NULL, description = NULL, browser_options = NULL,
                          info = NULL, dimension_links = NULL, category = NULL,
                          store = NULL, browser = NULL, ...) {

      callSuper(name = name, label = label, description = description, info = info)



      if (!is.null(dimensions) && !is.null(dimension_links)) {
        stop("ModelError: Both dimensions and dimension_links provided, use only one.")
      }

      # Physical properties
      mappings <<- mappings
      fact <<- fact
      joins <<- joins
      key <<- key
      browser_options <<- nvl(browser_options, list())
      browser <<- browser

      dimension_links <<- list() # OrderedDict

      for (link in expand_dimension_links(nvl(dimension_links, list()))) {
        link = as.list(link)
        dimension_links[[link$name]] <<- link
      }

      # Run-time properties
      # Sets in the Namespace.cube() when cube is created
      # Used by workspace internally to search for dimensions
      if (!is.null(store) && is.character(store)) {
        store_name <<- store
        store <<- NULL
      } else {
        store_name <<- store
        store <<- store
      }


      # TODO: make 'name' to be basename and ref to be full cube reference,
      # Be conistent!
      # Used by backends
      basename <<- name

      pvt_dimensions <<- list()

      if (!is.null(dimensions)) {
        if (!all(sapply(dimensions, function(dim){'Dimension' %in% class(dim)}))) {
          stop("ModelError: Dimensions for cube initialization should be a list of Dimension instances.")
        } else {
          for (dim in dimensions) {
            pvt_add_dimension(dim)
          }
        }
      }

      #
      # Prepare attributes
      # ------------------
      #
      # Measures

      measures = nvl(measures, list())
      assert_all_instances(measures, Measure, "measure")
      pvt_measures <<- object_dict(measures)

      # Aggregates
      #
      aggregates = nvl(aggregates, list())
      assert_all_instances(aggregates, MeasureAggregate, "aggregate")

      pvt_aggregates <<- object_dict(aggregates)

      # We don't need to access details by name
      details = nvl(details, list())
      assert_all_instances(details, Attribute, "detail")
      pvt_details <<- details
    },

    measures = function() {
      pvt_measures
    },

    measure = function(loc_name) {
      # Get measure object. If `obj` is a string, then measure with given
      # name is returned, otherwise measure object is returned if it belongs
      # to the cube. Returned object is of `Measure` type.
      #
      # Raises `NoSuchAttributeError` when there is no such measure or when
      # there are multiple measures with the same name (which also means that
      # the model is not valid).

      loc_name = as.character(loc_name)

      if (is.null(pvt_measures[[loc_name]])) {
        stop(sprintf("NoSuchAttributeError: Cube '%s' has no measure '%s'", name, loc_name))
      }

      pvt_measures[[loc_name]]
    },

    get_measures = function(loc_measures) {
      # """Get a list of measures as `Attribute` objects. If `measures` is
      #   `None` then all cube's measures are returned."""

      res = c()

      if (is.null(loc_measures)) {
        loc_measures = measures()
      }

      for (measure in loc_measures ) {
        res = append(res, measure)
      }

      res
    },

    aggregates = function() {
      pvt_aggregates
    },

    aggregate = function(loc_name) {
      # """Get aggregate object. If `obj` is a string, then aggregate with
      #   given name is returned, otherwise aggregate object is returned if it
      #   belongs to the cube. Returned object is of `MeasureAggregate` type.
      #
      #   Raises `NoSuchAttributeError` when there is no such aggregate or when
      #   there are multiple aggregates with the same name (which also means
      #   that the model is not valid).
      #   """

      loc_name = as.character(loc_name)

      if (is.null(pvt_aggregates[[loc_name]])) {
        stop(sprintf("NoSuchAttributeError: Cube '%s' has no measure aggregate '%s'", name, loc_name))
      }

      pvt_aggregates[[loc_name]]
    },

    get_aggregates = function(names=NULL) {
      # """Get a list of aggregates with `names`."""

      if (is.null(names)) {
        aggregates()
      } else {
        pvt_aggregates[names]
      }
    },

    aggregates_for_measure = function(mname) {
      # """Returns aggregtates for measure with `name`. Only direct function
      #   aggregates are returned. If the measure is specified in an expression,
      #   the aggregate is not included in the returned list"""

      aggs = aggregates()

      res = c()

      for (agg in aggs) {
        if (agg$measure == mname) {
          res = append(res, agg)
        }
      }

      res
    },

    all_dimension_keys = function() {
      # Returns all attributes that represent keys of dimensions and their
      # levels..
      attributes = c()
      for (dim in dimensions()) {
        attributes = append(attributes, dim$key_attributes)
      }
      attributes
    },

    all_attributes = function() {
      # """All cube's attributes: attributes of dimensions, details, measures
      #   and aggregates. Use this method if you need to prepare structures for
      #   any kind of query. For attributes for more specific types of queries
      #   refer to :meth:`Cube.all_fact_attributes` and
      #   :meth:`Cube.all_aggregate_attributes`.
      #
      #   .. versionchanged:: 1.1
      #
      #       Returns all attributes, including aggregates. Original
      #       functionality is available as `all_fact_attributes()`

      attributes = c()
      for (dim in getDimensions()) {
        attributes = append(attributes, dim$attributes())
      }

      attributes = append(attributes, pvt_details)
      attributes = append(attributes, pvt_measures)
      attributes = append(attributes, pvt_aggregates)

      names(attributes) <- NULL

      attributes
    },

    base_attributes = function() {
      # """Returns a list of attributes that are not derived from other
      #   attributes, do not depend on other cube attributes, variables or
      #   parameters. Any attribute that has an expression (regardless of it's
      #   contents, it might be a constant) is considered derived attribute.
      #
      #   The list contains also aggregate attributes that are base – for
      #   example attributes that represent pre-aggregated column in a table.
      #
      #   .. versionadded:: 1.1
      #   """

      res = c()

      attrs = all_attributes()

      attrs[sapply(attrs, function(attr)attr$is_base())]

    },

    all_fact_attributes = function() {
      # """All cube's attributes from the fact: attributes of dimensions,
      #   details and measures.
      #
      #   .. versionadded:: 1.1
      #   """

      attributes = c()
      for (dim in getDimensions()) {
        attributes = append(attributes, dim$attributes())
      }

      attributes = append(attributes, getDetails())
      attributes = append(attributes, getMeasures())

      names(attributes) <- NULL

      attributes
    },

    attribute_dependencies = function() {
      # """Dictionary of dependencies between attributes. Values are
      #   references of attributes that the key attribute depends on. For
      #   example for attribute `a` which has expression `b + c` the dictionary
      #   would be: `{"a": ["b", "c"]}`. The result dictionary includes all
      #   cubes' attributes and aggregates.
      #
      #   .. versionadded:: 1.1
      #   """

      attributes = append(all_attributes(), all_aggregate_attributes())

      res = list()

      for (attr in attributes) {
        res[[attr$ref]] = attr$dependencies
      }

      res
    },

    all_aggregate_attributes = function() {
      # """All cube's attributes for aggregation: attributes of dimensions and
      #   aggregates.  """

      attributes = c()

      for (dim in dimensions()) {
        attributes = append(attributes, dim$attributes())
      }

      attributes = append(attributes, aggregates())

      attributes
    },

    attribute = function(attribute) {
      # Returns an attribute object (dimension attribute, measure or
      # detail).

      # TODO: This should be a dictionary once the Cube object becomes
      # immutable

      attr_name = as.character(attribute)

      for (dim in getDimensions()) {
        attr = dim$attribute(attr_name, by_ref=TRUE, throw_error = FALSE)

        if (!is.null(attr)) {
          return(attr)
        }
      }

      for (detail in getDetails()) {
        if (detail$name == attr_name) {
          return(detail)
        }
      }

      for (measure in getMeasures()) {
        if (measure$name == attr_name) {
          return(measure)
        }
      }
      stop(sprintf("NoSuchAttributeError: Cube '%s' has no attribute '%s'", .self$name, attr_name))
    },

    get_attributes = function(attributes=NULL, aggregated=FALSE) {
      # """Returns a list of cube's attributes. If `aggregated` is `True` then
      # attributes after aggregation are returned, otherwise attributes for a
      # fact are considered.
      #
      # Aggregated attributes contain: dimension attributes and aggregates.
      # Fact attributes contain: dimension attributes, fact details and fact
      # measures.
      #
      # If the list `attributes` is empty, all attributes are returned.
      #
      # If `simplified_references` is `True` then dimension attribute
      # references in `attrubutes` are considered simplified, otherwise they
      # are considered as full (dim.attribute)."""

      # TODO: this should be a dictionary created in __init__ once this
      # class becomes immutable

      if (is.null(attributes)) {
        if (aggregated) {
          # return
          result = all_aggregate_attributes()
        } else {
          # return
          result = all_fact_attributes()
        }
      } else {
        everything = object_dict(all_attributes(), TRUE)

        names = sapply(nvl(attributes, list()), as.character)

        result = c()
        for (loc_name in names) {
          if (is.null(everything[[loc_name]])) {
            stop(sprintf("NoSuchAttributeError: Unknown attribute '%s' in cube '%s'", loc_name, name))
          }
          attr = everything[[loc_name]]

          result = append(result, attr)
        }

      }
      result
    },

    link_dimension = function(dimension) {
      # Links `dimension` object or a clone of it to the cube according to
      # the specification of cube's dimension link. See
      # :meth:`Dimension.clone` for more information about cloning a
      # dimension.

      link = dimension_links[[dimension$name]]

      if (!is.null(link)) {
        dimension = do.call(dimension$clone, link)
      }

      pvt_add_dimension(dimension)
    },

    # TODO: this method should be used only during object initialization
    pvt_add_dimension = function(dimension) {
        # """Add dimension to cube. Replace dimension with same name. Raises
        #   `ModelInconsistencyError` when dimension with same name already exists
        #   in the receiver. """

      if (is.null(dimension)) {
        stop(sprintf("ArgumentError: Trying to add None dimension to cube '%s'.", name))
      } else if (! ('Dimension' %in% class(dimension)) ) {
        stop(sprintf("ArgumentError: Dimension added to cube '%s' is not a Dimension instance. It is '%s'",
                     name,
                     typeof(dimension)))
      }

      pvt_dimensions[[dimension$name]] <<- dimension
    },

    getMeasures = function() {
      pvt_measures
    },

    getDimensions = function() {
      pvt_dimensions
    },

    dimension = function(obj) {
      #   Get dimension object. If `obj` is a string, then dimension with
      #   given name is returned, otherwise dimension object is returned if it
      #   belongs to the cube.
      #   Raises `NoSuchDimensionError` when there is no such dimension.

      # FIXME: raise better exception if dimension does not exist, but is in
      # the list of required dimensions

      if (is.null(obj))
        stop(sprintf("NoSuchDimensionError: Requested dimension should not be none (cube '%s')", name))

      loc_name = as.character(obj)

      if (is.null(pvt_dimensions[[loc_name]])) {
        stop(sprintf("NoSuchDimensionError: cube '%s' has no dimension '%s'", name, loc_name))
      }

      pvt_dimensions[[loc_name]]
    },

    distilled_hierarchies = function() {
      # """Returns a dictionary of hierarchies. Keys are hierarchy references
      #   and values are hierarchy level key attribute references.
      #
      #   .. warning::
      #
      #       This method might change in the future. Consider experimental."""
      #
      # hierarchies = {}
      # for dim in self.dimensions:
      #   for hier in dim.hierarchies:
      #   key = (dim.name, hier.name)
      #   levels = [hier_key.ref for hier_key in hier.keys()]
      #
      #   hierarchies[key] = levels
      #
      #   if dim.default_hierarchy_name == hier.name:
      #     hierarchies[(dim.name, None)] = levels
      #
      #   return hierarchies
    },

    to_dict = function(options = list()) {
      # """Convert to a dictionary. If `with_mappings` is ``True`` (which is
      # default) then `joins`, `mappings`, `fact` and `options` are included.
      # Should be set to ``False`` when returning a dictionary that will be
      # provided in an user interface or through server API.

      out = callSuper(options)

      out$locale = locale
      out$category = category

      aggregates = lapply(pvt_aggregates, function(a){a$to_dict(options=options)})
      out$aggregates = aggregates

      measures = lapply(pvt_measures, function(m){m$to_dict(options=options)})
      out$measures = measures

      details = lapply(pvt_details, function(d){d$to_dict(options=options)})
      out$details = details

      if (nvl(options$expand_dimensions, FALSE)) {
        limits = list()

        # TODO: move this to metadata as strip_hierarchies()
        hierarchy_limits = nvl(options$hierarchy_limits, c())

        # for (dim, hier, level in hierarchy_limits)
        #   limits[dim][hier] = level
        limits = hierarchy_limits

        dims = c()

        for (dim in getDimensions()) {
          limit = limits[[dim$name]]
          dinfo = dim$to_dict(list(hierarchy_limits=limit))
          dims = append(dims, dinfo)
        }

      } else {
        dims = sapply(getDimensions(), function(dim){dim$name})
      }

      out$dimensions = dims

      if (nvl(options$with_mappings, FALSE)) {
        out$mappings = mappings
        out$fact = fact
        out$joins = joins
        out$browser_options = browser_options
      }

      out$key = key

      out
    },


    getDetails = function() {
      pvt_details
    }



  )
)

Cube.from_metadata = function(metadata) {
  #   Create a cube object from `metadata` dictionary. The cube has no
  # dimensions attached after creation. You should link the dimensions to the
  # cube according to the `Cube.dimension_links` property using
  # `Cube._add_dimension()`


  if (is.null(metadata$name))
    stop("ModelError:Cube metadata has no name")

  metadata = expand_cube_metadata(metadata)
  c.dimension_links = metadata$dimensions
  metadata$dimensions = NULL

  if (is.null(metadata$measures) && is.null(metadata$aggregates)) {
    metadata$aggregates = list(DEFAULT_FACT_COUNT_AGGREGATE)
  }

  # Prepare aggregate and measure lists, do implicit merging

  c.details = create_list_of(Attribute, metadata$details)
  c.measures = create_list_of(Measure, metadata$measures)

  # Inherit the nonadditive property in each measure
  nonadditive = metadata$nonadditive
  metadata$nonadditive = NULL
  if (!is.null(nonadditive)) {
    for (measure_item in c.measures) {
      measure_item$nonadditive <- nonadditive
    }
  }

  c.aggregates = create_list_of(MeasureAggregate, metadata$aggregates)
  metadata$aggregates = NULL

  aggregate_dict = list()
  measure_dict = list()

  for (a in c.aggregates) {
    aggregate_dict[[a$name]] = a
  }
  for (m in c.measures) {
    measure_dict[[m$name]] = m
  }

  # TODO: Depreciate?
  if (nvl(metadata$implicit_aggregates, F)) {
    implicit_aggregates = c()

    for (measure in c.measures) {
      implicit_aggregates = append(implicit_aggregates, measure$default_aggregates())
    }

    for (aggregate in implicit_aggregates) {
      skip = FALSE
      # an existing aggregate either has the same name,
      existing = aggregate_dict[[aggregate$name]]
      if (!is.null(existing)) {
        if (existing$FUN != aggregate$FUN) {
          stop(sprintf("ModelError: Aggregate '%s' function mismatch. Implicit function %s, explicit function: %s.",
                       aggregate$name,aggregate$FUN,existing$FUN))
        }
        skip = TRUE
      } else {
        # or the same function and measure

        existing = any(sapply(c.aggregates, function(agg){
          nvl(agg$FUN, '') == nvl(aggregate$FUN, '') && nvl(agg$measure, '') == nvl(measure$name, '')
        }))

        if (existing) {
          skip = TRUE
        }
      }

      if (!skip) {
        c.aggregates = append(c.aggregates, aggregate)
        aggregate_dict[[aggregate$name]] = aggregate
      }
    }
  }

  # Assign implicit aggregate labels
  # TODO: make this configurable
  for (aggregate in aggregate_dict) {
    measure = measure_dict[[nvl(aggregate$measure, '')]]
    if (is.null(measure)) {
      measure = aggregate_dict[[nvl(aggregate$measure, '')]]
    }

    if (is.null(aggregate$label)) {
      aggregate$label = pvt_measure_aggregate_label(aggregate, measure)
    }

    # Inherit nonadditive property from the measure
    if (!is.null(measure) && is.null(aggregate$nonadditive)) {
      aggregate$nonadditive = measure$nonadditive
    }

    aggregate_dict[[aggregate$name]] = aggregate
  }


  metadata$measures=measure_dict
  metadata$aggregates=aggregate_dict
  metadata$dimension_links=c.dimension_links
  metadata$details=c.details

  do.call(Cube$new, metadata)

}

pvt_measure_aggregate_label <- function(aggregate, measure) {
  FUN = aggregate$FUN
  template = nvl(IMPLICIT_AGGREGATE_LABELS[[nvl(FUN, '')]], "%s")

  if (is.null(aggregate$label)) {
    if (!is.null(measure)) {
      measure_label = nvl(measure$label, measure$name)
    } else {
      if (!is.null(aggregate$measure)) {
        measure_label = aggregate$measure
      } else {
        measure_label = aggregate$name
      }
    }

    label = sprintf(template, measure_label)
  }

  label
}


expand_dimension_links <- function(metadata) {
  # Expands links to dimensions. `metadata` should be a list of strings or
  # dictionaries (might be mixed). Returns a list of dictionaries with at
  # least one key `name`. Other keys are: `hierarchies`,
  # `default_hierarchy_name`, `nonadditive`, `cardinality`, `template`

  #lapply(metadata, function() {
  # TODO it might be needed to refactor it with lapply
  #})
  links = list()

  for (link in metadata) {
    if (is.character(link)) {
      link = list(name = link)
    } else if (is.null(link$name)) {
      stop('ModelError: Dimension link has no name')
    }
    links[[length(links) + 1]] = link
  }

  links
}


expand_cube_metadata <- function(metadata) {
  #   Expands `metadata` to be as complete as possible cube metadata.
  # `metadata` should be a dictionary.

  if (is.null(metadata$name))
    stop("ModelError:Cube has no name")

  if (is.null(metadata$dimensions)) {
    links = expand_dimension_links(metadata$dimensions)
  } else {
    links = metadata$dimensions
  }

  nonadditive = metadata$nonadditive

  if (!is.null(metadata$measures)) {

    measures = c()

    if (!is.null(names(metadata$measures))) {
      metadata$measures = list(metadata$measures)
    }

    metadata$measures = lapply(metadata$measures, expand_attribute_metadata)
  }

  if (length(links) > 0) {
    metadata$dimensions = links
  }

  metadata
}

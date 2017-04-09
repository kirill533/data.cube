DEFAULT_FACT_COUNT_AGGREGATE = list(
  "name" = "fact_count",
  "label" = "Count",
  "function" = "count"
)


expand_attribute_metadata <- function(metadata) {
  # Fixes metadata of an attribute. If `metadata` is a string it will be
  #   converted into a dictionary with key `"name"` set to the string value.

  if (is.character(metadata)) {
    metadata = list("name"= metadata)
  }

  metadata
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


Cube = setRefClass(
  'Cube',

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
    metadata$aggregates = c(DEFAULT_FACT_COUNT_AGGREGATE)
  }

  # Prepare aggregate and measure lists, do implicit merging

  c.details = create_list_of(Attribute, metadata$details)
  c.measures = create_list_of(Measure, metadata$measures)

  # Inherit the nonadditive property in each measure
  nonadditive = metadata$nonadditive
  metadata$nonadditive = NULL
  if (!is.null(nonadditive)) {
    for (measure_name in names(c.measures)) {
      c.measures[[measure_name]]$nonadditive = nonadditive
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

  metadata$measures=measure_dict
  metadata$aggregates=aggregate_dict
  metadata$dimension_links=c.dimension_links
  metadata$details=c.details

  do.call(Cube$new, metadata)

}

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

  links = c()

  for (link in metadata) {
    if (is.character(link)) {
      full_link = list(name = link)
    } else if (is.null(link$name)) {
      stop('ModelError: Dimension link has no name')
    }
    links = c(links, link)
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

    for (attr in metadata$measures) {
      attr = expand_attribute_metadata(attr)
      if (!is.null(nonadditive) && is.null(attr$nonadditive)) {
        attr$nonadditive = nonadditive
      }

      measures = c(measures, attr)
    }
    metadata$measures = measures
  }

  if (length(links) > 0) {
    metadata$dimensions = links
  }

  metadata
}


Cube = setRefClass(
  'Cube',

  fields = c('name', 'dimensions', 'measures', 'aggregates',
             'label', 'details', 'mappings', 'joins',
             'fact', 'key', 'description', 'browser_options',
             'info', 'dimension_links', 'locale', 'category',
             'store', 'options'),

  methods = list(


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

  if (is.null(metadata$measures) && is.null(metadata$aggregates)) {
    metadata$aggregates = c(DEFAULT_FACT_COUNT_AGGREGATE)
  }

  # Prepare aggregate and measure lists, do implicit merging

  c.details = create_list_of(Attribute, metadata$details)
  c.measures = create_list_of(Measure, metadata$measures)

  # Inherit the nonadditive property in each measure
  nonadditive = metadata$nonadditive
  if (!is.null(nonadditive)) {
    for (measure_name in names(c.measures)) {
      c.measures[[measure_name]]$nonadditive = nonadditive
    }
  }

  c.aggregates = create_list_of(MeasureAggregate, metadata$aggregates)

  aggregate_dict = list()
  measure_dict = list()
  for (a in c.aggregates) {
    aggregate_dict[[a$name]] = a
  }
  for (m in c.measures) {
    measure_dict[[m$name]] = m
  }

  Cube$new(measures=measure_dict,
          aggregates=aggregate_dict,
          dimension_links=c.dimension_links,
          details=c.details,
          options=metadata)

}

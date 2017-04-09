DEFAULT_KEY_FIELD = "id"

DEFAULT_FACT_KEY = 'id'
DEFAULT_DIMENSION_KEY = 'id'

NAMING_DEFAULTS = list(
  "fact_prefix" = NULL,
  "fact_suffix" = NULL,
  "dimension_prefix" = NULL,
  "dimension_suffix" = NULL,
  "dimension_key_prefix" = NULL,
  "dimension_key_suffix" = NULL,

  "denormalized_prefix" = NULL,
  "denormalized_suffix" = NULL,

  "aggregated_prefix" = NULL,
  "aggregated_suffix" = NULL,

  "fact_key" = DEFAULT_FACT_KEY,
  "dimension_key" = DEFAULT_DIMENSION_KEY,
  "explicit_dimension_primary" = F,

  "schema" = NULL,
  "fact_schema" = NULL,
  "dimension_schema" = NULL,
  "aggregate_schema" = NULL
)

distill_naming <- function(dictionary) {
  d = list()
  # Distill only keys and values related to the naming conventions.
  for (n in names(dictionary)) {
    if (!(n %in% names(NAMING_DEFAULTS))) {
      dictionary[[n]] = NULL
    }
  }

  do.call(Naming$new, dictionary)
}

privat_match_names = function(pattern, names) {
  # Match names to patterns and return a tuple of matching name with
  # extracted value (stripped of suffix/prefix).

  result = c()

  split = unlist(strsplit(pattern, '{name}', fixed = T))

  names = names[stringi::stri_startswith_fixed(names, split[1]) & stringi::stri_endswith_fixed(names, split[2])]

  data.frame(
    full_name = names,
    name = substr(full_name, length(split[1]), length(full_name) - length(split[2]))
  )
}


# Naming conventions for SQL tables. Naming properties can be accessed as
# a dictionary keys or as direct attributes. The naming properties are:
#
# * `fact_prefix` – prefix for fact tables
# * `fact_suffix` – suffix for fact tables
# * `dimension_prefix` – prefix for dimension tables
# * `dimension_suffix` – suffix for dimension tables
# * `dimension_key_prefix` – prefix for dimension foreign keys
# * `dimension_key_suffix` – suffix for dimension foreign keys
# * `fact_key` – name of fact table primary key (defaults to ``id`` if not
# specified)
# * `dimension_key` – name of dimension table primary key (defaults to
# ``id`` if not specified)
# * `explicit_dimension_primary` – whether the primary key of dimension
# table contains dimension name explicitly.
#
# If the `explicit_dimension_primary` is `True`, then all dimension tables
# are expected to have the primary key in the same format as foreign
# dimension keys. For example if the foreign dimension keys are
# ``customer_key`` then primary key of customer dimension table is also
# ``customer_key`` as oposed to just ``key``. The `dimension_key` naming
# property is ignored.
#
#
# Additional information that can be used by the mapper:
#
# * `schema` – default schema
# * `fact_schema` – schema where all fact tables are stored
# * `dimension_schema` – schema where dimension tables are stored
#
# Recommended values: `fact_prefix` = ``ft_``, `dimension_prefix` =
# ``dm_``, `explicit_dimension_primary` = ``True``.
Naming = setRefClass(
  'Naming',
  fields = c(
    'dimension_prefix', 'dimension_suffix', 'dim_name_pattern',
    'fact_name_pattern', 'fact_prefix', 'fact_suffix',
    'dim_key_pattern', 'dimension_key_prefix', 'dimension_key_suffix',
    'denormalized_prefix',
    'denormalized_suffix',
    'aggregated_prefix',
    'aggregated_suffix',
    'fact_key',
    'dimension_key',
    'explicit_dimension_primary',
    'schema',
    'fact_schema',
    'dimension_schema',
    'aggregate_schema'
  ),
  methods = list(

    initialize = function(dimension_prefix = NULL, dimension_suffix = NULL, dim_name_pattern = NULL,
                          fact_name_pattern = NULL, fact_prefix = NULL, fact_suffix = NULL,
                          dim_key_pattern = NULL, dimension_key_prefix = NULL, dimension_key_suffix = NULL,
                          denormalized_prefix = NULL,
                          denormalized_suffix = NULL,
                          aggregated_prefix = NULL,
                          aggregated_suffix = NULL,
                          fact_key = NULL,
                          dimension_key = NULL,
                          explicit_dimension_primary = NULL,
                          schema = NULL,
                          fact_schema = NULL,
                          dimension_schema = NULL,
                          aggregate_schema = NULL) {
      # Creates a `Naming` object instance from a dictionary. If `fact_key`
      # or `dimension_key` are not specified, then they are set to ``id`` by
      # default.

      callSuper(dimension_prefix = dimension_prefix, dimension_suffix = dimension_suffix, dim_name_pattern = dim_name_pattern,
                fact_name_pattern = fact_name_pattern, fact_prefix = fact_prefix, fact_suffix = fact_suffix,
                dim_key_pattern = dim_key_pattern, dimension_key_prefix = dimension_key_prefix, dimension_key_suffix = dimension_key_suffix,
                denormalized_prefix = denormalized_prefix,
                denormalized_suffix = denormalized_suffix,
                aggregated_prefix = aggregated_prefix,
                aggregated_suffix = aggregated_suffix,
                fact_key = fact_key,
                dimension_key = dimension_key,
                explicit_dimension_primary = explicit_dimension_primary,
                schema = schema,
                fact_schema = fact_schema,
                dimension_schema = dimension_schema,
                aggregate_schema = aggregate_schema)

      # Set the defaults
      for (key in names(NAMING_DEFAULTS)) {
        if (is.null(.self[[key]])) {
          .self[[key]] = NAMING_DEFAULTS[[key]]
        }
      }

      dim_name_pattern <<- paste0(dimension_prefix, '{name}', dimension_suffix)

      fact_name_pattern <<- paste0(fact_prefix, '{name}', fact_suffix)

      dim_key_pattern <<- paste0(dimension_key_prefix, '{name}', dimension_key_suffix)

    },

    dimension_table_name = function(name) {
      if (!is.character(name))
        name = name$name
      # Constructs a physical dimension table name for dimension `name`
      sprintf('%s%s%s', nvl(dimension_prefix, ''), name, nvl(dimension_suffix, ''))
    },

    fact_table_name = function(name) {
      if (!is.character(name))
        name = name$name
      # Constructs a physical fact table name for fact/cube `name`
      sprintf('%s%s%s', nvl(fact_prefix, ''), name, nvl(fact_suffix, ''))
    },

    denormalized_table_name = function(name) {
      if (!is.character(name))
        name = name$name
      # Constructs a physical fact table name for fact/cube `name`
      sprintf('%s%s%s', nvl(denormalized_prefix, ''), name, nvl(denormalized_suffix, ''))
    },

    # TODO: require list of dimensions here
    aggregated_table_name = function(name) {
      if (!is.character(name))
        name = name$name
      # Constructs a physical fact table name for fact/cube `name`
      paste0(aggregated_prefix, name, aggregated_suffix)
    },

    dimension_primary_key = function(name) {
      if (!is.character(name))
        name = name$name
      # Constructs a dimension primary key name for dimension `name`

      if (explicit_dimension_primary) {
        paste0(dimension_key_prefix, name, dimension_key_suffix)
      } else {
        dimension_key
      }
    },

    dimension_keys = function(keys) {
      # Return a list of tuples (`key`, `dimension`) for every key in
      # `keys` that matches dimension key naming. Useful when trying to
      # identify dimensions and their foreign keys in a fact table that
      # follows the naming convetion.

      privat_match_names(dim_key_pattern, keys)
    },

    dimensions = function(table_names) {
      # Return a list of tuples (`table`, `dimension`) for all tables that
      # match dimension naming scheme. Usefult when trying to identify
      # dimension tables in a database that follow the naming convention.

      private_match_names(dim_name_pattern, table_names)
    },

    facts = function(table_names) {
      # Return a list of tuples (`table`, `fact`) for all tables that
      # match fact table naming scheme. Useful when trying to identify fact
      # tables in a database that follow the naming convention.

      private_match_names(fact_name_pattern, table_names)
    }
  )
)

# A dictionary-like object that provides physical column references for
# cube attributes. Does implicit mapping of an attribute.
Mapper = setRefClass(
  'Mapper',
  fields = c('cube', 'naming', 'mappings', 'fact_name'),
  methods = list(
    initialize = function(cube, naming) {
      # Creates a mapping for `cube` using `naming` conventions within
      # optional `locale`. `naming` has to be a :class:`cubes.Naming`
      # object.

      callSuper()

      naming <<- naming
      mappings <<- nvl(cube$mappings, list())
      fact_name <<- nvl(cube$fact, naming$fact_table_name(cube$name))
    },

    getitem = function(attribute) {
      # Returns implicit physical column reference for `attribute`, which
      # should be an instance of :class:`cubes.model.Attribute`. If there is
      # no dimension specified in attribute, then fact table is assumed. The
      # returned reference has attributes `schema`, `table`, `column`,
      # `extract`.

      tbl = attribute_table(attribute)

      tbl$column = attribute$name

      to_column(tbl)
    },

    attribute_table = function(attribute) {
      # Return a tuple (schema, table) for attribute.

      dimension = attribute$dimension

      if (!is.null(dimension)) {
        schema = nvl(naming$dimension_schema, naming$schema)

        if (dimension$is_flat() && !dimension$has_details()) {
          table = fact_name
        } else {
          table = naming$dimension_table_name(dimension)
        }
      } else {
        table = fact_name
        schema = naming$schema
      }

      list(schema = schema, table = table)
    }
  )
)

DenormalizedMapper = setRefClass(
  'DenormalizedMapper',
  contains = 'Mapper',
  methods = list(
    getitem = function(attribute) {
      if (attribute$expression) {
        stop(strpintf("Attribute '%s' has an expression, it can not have a direct physical representation", attribute$name))
      }

      callSuper(attribute)
    }
  )
)

StarSchemaMapper = setRefClass(
  'StarSchemaMapper',
  contains = 'Mapper',
  methods = list(
    getitem = function(attribute) {
      # Find physical reference for a star schema as follows:
      #
      # 1. if there is mapping for `dimension.attribute`, use the mapping
      # 2. if there is no mapping or no mapping was found, then use table
      # `dimension` or fact table, if attribute does not belong to a
      # dimension and column `attribute`
      #
      # If table prefixes and suffixes are used, then they are
      # prepended/appended to the table tame in the implicit mapping.
      #
      # If localization is requested and the attribute is localizable, then
      # suffix in the form `_LOCALE` where `LOCALE` is the locale name will be
      # added to search for mapping or for implicit attribute creation such as
      # `name_sk` for attribute `name` and locale `sk`.

      if (!is.null(attribute$expression)) {
        stop(strpintf("Attribute '%s' has an expression, it can not have a direct physical representation", attribute$name))
      }

      logical = attribute$ref
      physical = mappings[[logical]]

      if (!is.null(physical)) {
        # TODO: Should we not get defaults here somehow?
        to_column(physical)
      } else {
        # No mappings exist or no mapping was found - we are going to create
        # default physical reference
        callSuper(attribute)
      }
    }
  )
)


map_base_attributes = function(cube, mapper_class, naming) {
  # Map all base attributes of `cube` using mapping function `mapper`.
  # `naming` is a naming convention object. Returns a tuple (`fact_name`,
  # `mapping`) where `fact_name` is a fact table name and `mapping` is a
  # dictionary of attribute references and their physical column
  # references.

  base = cube$all_attributes[cube$all_attributes$is_base]

  mapper = mapper_class$new(cube, naming)
  # mapped = {attr.ref: for attr in base}
  mapped = mapper$getitem(base)

  data.frame(fact_name = fact_name, mapped = mapped)
}



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

  Naming$new(config=dictionary)
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
  fields = c('config'),
  methods = list(

  )
)

Mapper = setRefClass(
  'Mapper',
  fields = c('cube', 'naming'),
  methods = list(

  )
)

DenormalizedMapper = setRefClass(
  'DenormalizedMapper',
  contains = 'Mapper'
)

StarSchemaMapper = setRefClass(
  'StarSchemaMapper',
  contains = 'Mapper'
)

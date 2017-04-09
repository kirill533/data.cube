# Proposed Provider API:
#     Provider.cube() – in abstract class
#     Provider.provide_cube() – in concrete class, providers Cube object that
#         might be modified later
#     Provider.provide_dimension()
#     Provider.link_cube(cube,locale)
#     Provider.find_dimension(cube, locale)
#
# Provider is bound to namespace

# TODO: add tests
# TODO: needs to be reviewed
link_cube <- function(cube, locale, provider=NULL, namespace=NULL,
              ignore_missing=FALSE) {
  # Links dimensions to the `cube` in the `context` object. The `context`
  # object should implement a function `dimension(name, locale, namespace,
  # provider)`. Modifies cube in place, returns the cube.

  # TODO: change this to: link_cube(cube, locale, namespace, provider)

  # Assumption: empty cube

  linked = c()

  for (dim_name in names(cube$dimension_links)) {
    if (dim_name %in% linked)
      stop(sprintf("ModelError: Dimension '%s' linked twice", dim_name))

    dim = find_dimension(dim_name,
                         provider=provider,
                         namespace=namespace)

    if ((is.null(dim)) && !ignore_missing) {
      stop(sprintf("CubesError: Dimension '%s' not found.", dim_name))
    }

    cube$link_dimension(dim)
  }

  cube
}

# TODO: add tests
find_dimension <- function(name, provider=NULL, namespace=NULL) {
  # Returns a localized dimension with `name`. Raises
  # `NoSuchDimensionError` when no model published the dimension. Raises
  # `RequiresTemplate` error when model provider requires a template to be
  # able to provide the dimension, but such template is not a public
  # dimension.
  #
  # The standard lookup when linking a cube is:
  #
  # 1. look in the provider
  # 2. look in the namespace – all providers within that namespace

  # Collected dimensions – to be used as templates
  templates = list()

  # Assumption: all dimensions that are to be used as templates should
  # be public dimensions. If it is a private dimension, then the
  # provider should handle the case by itself.
  missing = c(name)


  while (length(missing) > 0) {
    dimension = NULL
    len = length(missing)

    name = missing[len]
    missing = missing[0:(len-1)]

    # First give a chance to provider, then to namespace
    dimension = NULL
    required_template = NULL

    tryCatch({
      dimension = pvt_lookup_dimension(name, templates,
                                       namespace, provider)
    }, error = function(e) {
      # except TemplateRequired as e:
      # required_template = e.template
      required_template = 'some_template'
    }, finally = {

    })

    if (!is.null(required_template)) {

      stop('required_template implementation is not finished')

      # if required_template in templates:
      #   raise BackendError("Some model provider didn't make use of "
      #                      "dimension template '%s' for '%s'"
      #                      % (required_template, name))

      # if required_template in templates:
      #   raise BackendError("Some model provider didn't make use of "
      #                      "dimension template '%s' for '%s'"
      #                      % (required_template, name))

      # missing.append(name)
      #   if required_template in missing:
      #       raise ModelError("Dimension templates cycle in '%s'" %
      #                        required_template)
      #   missing.append(required_template)

    }

    # Store the created dimension to be used as template
    if (!is.null(dimension)) {
      templates[[name]] = dimension
    }
  }


  if (!is.null(namespace)) {
    warning('namespaces are not supported')
    # lookup = namespace$translation_lookup(locale)

    # if lookup:
      # TODO: pass lookup instead of jsut first found translation
      # context = LocalizationContext(lookup[0])
      # trans = context.object_localization("dimensions", "inner")
      # dimension = dimension.localized(trans)
  }

  dimension
}

# TODO: add tests
pvt_lookup_dimension = function(name, templates, namespace, provider) {
  # """Look-up a dimension `name` in `provider` and then in `namespace`.
  #
  # `templates` is a dictionary with already instantiated dimensions that
  # can be used as templates.
  # """

  dimension = NULL

  # 1. look in the povider
  if (!is.null(provider)) {
    tryCatch({

      dimension = provider$dimension(name, templates=templates)

      return(dimension)

    }, error = function(e) {
      # NoSuchDimensionError
    }, finally = {

    })
  }

  # 2. Look in the namespace
  if (!is.null(namespace)) {
    return(namespace$dimension(name, templates=templates))
  }

  stop(sprintf("NoSuchDimensionError: Dimension '%s' not found", name))
}





listMerge <- function (list1, list2) {
  allNames <- unique(c(names(list1), names(list2)))
  merged <- list1 # we will copy over/replace values from list2 as necessary
  for (x in allNames) {
    # convenience
    a <- list1[[x]]
    b <- list2[[x]]
    if (is.null(a)) {
      # only exists in list2, copy over
      merged[[x]] <- b
    } else if (is.list(a) && is.list(b)) {
      # recurse
      merged[[x]] <- listMerge(a, b)
    } else if (!is.null(b)) {
      # replace the list1 value with the list2 value (if it exists)
      merged[[x]] <- b
    }
  }
  return(merged)
}

# Abstract class – factory for model object. Currently empty and used
# only to find other model providers.
ModelProvider = setRefClass(
  "ModelProvider",
  fields = c(
    'metadata', 'cubes_metadata', 'store', 'defaults', 'dimensions_metadata', 'options'
  ),
  methods = list(

    # TODO: Don't get metadata, but arbitrary arguments.
    initialize = function(metadata=NULL) {
      #   """Base class for model providers. Initializes a model provider and
      # sets `metadata` – a model metadata dictionary.
      #
      # Subclasses should call this method at the beginning of the custom
      # `__init__()`.
      #
      # If a model provider subclass has a metadata that should be pre-pended
      # to the user-provided metadta, it should return it in
      # `default_metadata()`.
      #
      # Subclasses should implement at least: :meth:`cubes.ModelProvider.cube`,
      # :meth:`cubes.ModelProvider.dimension` and
      # :meth:`cubes.ModelProvider.list_cubes` methods.
      # """

      store <<- NULL

      # Get provider's defaults and pre-pend it to the user provided
      # metadtata.
      defaults <<- default_metadata()
      metadata <<- merge_metadata(defaults, metadata)

      # TODO: check for duplicates
      dimensions_metadata <<- list()

      for (dim in metadata$dimensions) {
        dimensions_metadata[[dim$name]] <<- dim
      }

      cubes_metadata <<- list()

      for (cube in metadata$cubes) {
        cubes_metadata[[cube$name]] <<- cube
      }

      options <<- listMerge(metadata$options, options)
    },

    default_metadata = function(metadata=NULL) {
      list()
    },

    merge_metadata = function(meta, other) {
      # See `default_metadata()` for more information.

      meta = listMerge(meta, other)

      # TODO Implement logic of metadata merging

      # cubes = metadata.pop("cubes", []) + other.pop("cubes", [])
      # if cubes:
      #   metadata["cubes"] = cubes
      #
      # dims = metadata.pop("dimensions", []) + other.pop("dimensions", [])
      # if dims:
      #   metadata["dimensions"] = dims
      #
      # joins = metadata.pop("joins", []) + other.pop("joins",[])
      # if joins:
      #   metadata["joins"] = joins
      #
      # mappings = metadata.pop("mappings", {})
      # mappings.update(other.pop("mappings", {}))
      # if mappings:
      #   metadata["mappings"] = mappings
      #
      # metadata.update(other)

      meta
    },

    cube = function(name, namespace=NULL) {
      # Returns a cube with `name` provided by the receiver. If receiver
      # does not have the cube `NoSuchCube` exception is raised.
      #
      # Note: The returned cube will not have the dimensions assigned.
      # It is up to the caller's responsibility to assign appropriate
      #   dimensions based on the cube's `dimension_links`.
      #
      # Subclasses of `ModelProvider` might override this method if they would
      # like to create the `Cube` object directly.
      #
      # .. note:
      #
      #   If provider is caching a cube, it should store a cache for
      # localized version of the cube.


      meta = cube_metadata(name)
      cube = Cube.from_metadata(meta)
      link_cube(cube, provider=.self, namespace=namespace)

      cube
    },

    default_metadata = function(metadata=NULL) {
      # Returns metadata that are prepended to the provided model metadata.
      # `metadata` is user-provided metadata and might be used to decide what
      # kind of default metadata are returned.
      #
      # The metadata are merged as follows:
      #
      # * cube lists are concatenated (no duplicity checking)
      # * dimension lists are concatenated (no duplicity checking)
      # * joins are concatenated
      # * default mappings are updated with the model's mappings
      #
      # Default implementation returns empty metadata.

      list()
    },

    # TODO: bind this automatically on provider configuration: store (see
    # requires_store() function)
    bind = function(store) {
      # Set's the provider's `store`.

      store <<- store
      initialize_from_store()
    },

    initialize_from_store = function() {
      # This method is called after the provider's `store` was set.
      # Override this method if you would like to perform post-initialization
      # from the store.
    },


    cube_options = function(cube_name) {
      # Returns an options dictionary for cube `name`. The options
      # dictoinary is merged model `options` metadata with cube's `options`
      # metadata if exists. Cube overrides model's global (default)
      # options.

      if (!is.null(cubes_metadata[[cube_name]])) {
        cube = cubes_metadata[[cube_name]]
        # TODO: decide which one to use
        options <<- listMerge(options, nvl(cube$options, list()))
        options <<- listMerge(options, nvl(cube$browser_options, list()))
      }

      options
    },

    dimension_metadata = function(name, locale=None) {
      # Returns a metadata dictionary for dimension `name` and optional
      # `locale`.
      #
      # Subclasses should override this method and call the super if they
      # would like to merge metadata provided in a model file.

      if (!is.null(dimensions_metadata[[name]])) {
        dimensions_metadata[[name]]
      } else {
        stop(sprintf("No such dimension '%s'", name))
      }

    },

    cube_metadata = function(name) {
      # Returns a cube metadata by combining model's global metadata and
      #   cube's metadata. Merged metadata dictionaries: `browser_options`,
      # `mappings`, `joins`.
      #
      # Subclasses should override this method and call the super if they
      # would like to merge metadata provided in a model file.
      #
      # .. note:
      #
      # If provider is caching a cube metadata, it should store a cache
      # for localized version of the cube metadata.

      if (!is.null(cubes_metadata[[name]]))
        meta = cubes_metadata[[name]]
      else
        stop(sprintf("NoSuchCubeError:No such cube '%s'", name))

      meta = listMerge(meta, metadata)

      # TODO finish logic of metadata merging

      meta
    },

    list_cubes = function() {
      #   Get a list of metadata for cubes in the workspace. Result is a list
      #   of dictionaries with keys: `name`, `label`, `category`, `info`.
      #
      #   The list is fetched from the model providers on the call of this
      #   method.
      #
      #   Subclassees should implement this method.

      stop("Subclasses should implement list_cubes()")
    },

    has_cube = function(name) {
      # Returns `True` if the provider has cube `name`. Otherwise returns `False`.

      !is.null(cubes_metadata[[name]])
    },

    cube = function(name, locale=None, namespace=None) {
        # Returns a cube with `name` provided by the receiver. If receiver
        # does not have the cube `NoSuchCube` exception is raised.
        #
        # Note: The returned cube will not have the dimensions assigned.
        # It is up to the caller's responsibility to assign appropriate
        #     dimensions based on the cube's `dimension_links`.
        #
        # Subclasses of `ModelProvider` might override this method if they would
        # like to create the `Cube` object directly.
        #
        # .. note:
        #
        # If provider is caching a cube, it should store a cache for
        # localized version of the cube.

        metadata = cube_metadata(name, locale)
        cube = Cube$from_metadata(metadata)
        link_cube(cube, locale, provider=self, namespace=namespace)

        cube
    },

    dimension = function(name, templates=c()) {
      # Returns a dimension with `name` provided by the receiver.
      # `dimensions` is a dictionary of dimension objects where the receiver
      # can look for templates. If the dimension requires a template and the
      # template is missing, the subclasses should raise
      # `TemplateRequired(template)` error with a template name as an
      # argument.
      # If the receiver does not provide the dimension `NoSuchDimension`
      # exception is raised.

      meta = dimension_metadata(name)
      Dimension.from_metadata(metadata = meta, templates=templates)
    }


  )
)


StaticModelProvider = setRefClass('StaticModelProvider', contains = 'ModelProvider')



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

ModelProvider = setRefClass(
  "ModelProvider",
  fields = c(
    'metadata', 'cubes_metadata', 'store', 'defaults', 'dimensions_metadata', 'options'
  ),
  methods = list(

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

      cube
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
    }
  )
)


StaticModelProvider = setRefClass('StaticModelProvider', contains = 'ModelProvider')


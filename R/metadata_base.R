# Base classs for all model objects.
ModelObject = setRefClass(
  'ModelObject',
  fields = c('name', 'label', 'description', 'info'),
  methods = list(

    initialize = function(name=NULL, label=NULL, description=NULL, info=list(), ...) {
      # Initializes model object basics. Assures that the `info` is a dictionary.

      callSuper(...)

      name <<- name
      label <<- label
      description <<- description
      info <<- info


    },

    to_dict = function(options = list()) {
      # """Convert to a dictionary. If `with_mappings` is ``True`` (which is
      #   default) then `joins`, `mappings`, `fact` and `options` are included.
      #   Should be set to ``False`` when returning a dictionary that will be
      #   provided in an user interface or through server API.
      #   """

      out = list()

      out$name = name
      out$info = info

      if (nvl(options$create_label, FALSE)) {
        out$label = nvl(label, to_label(name))
      } else {
        out$label = label
      }

      out$description = description

      out
    },

    copy = function(shallow = FALSE) {
      def <- .refClassDef
      value <- new(def, name = .self$name, label = .self$label,
                   description = .self$description,
                   info = .self$info)
      vEnv <- as.environment(value)
      selfEnv <- as.environment(.self)
      for (field in names(def@fieldClasses)) {
        if (shallow)
          assign(field, get(field, envir = selfEnv), envir = vEnv)
        else {
          current <- get(field, envir = selfEnv)
          if (is(current, "envRefClass"))
            current <- current$copy(FALSE)
          assign(field, current, envir = vEnv)
        }
      }
      value
    }

  )
)


object_dict <- function(objects, by_ref=FALSE, error_message=NULL, error_dict=NULL) {
  # Make an ordered dictionary from model objects `objects` where keys are
  # object names. If `for_ref` is `True` then object's `ref` (reference) is
  # used instead of object name. Keys are supposed to be unique in the list,
  # otherwise an exception is raised.

  if (!is.list(objects)) {
    items = as.list(objects)
  } else {
    items = objects
  }

  if (by_ref) {
    names(items) <- sapply(objects, function(obj){obj$ref})
  } else {
    names(items) <- sapply(objects, function(obj){obj$name})
  }

  ordered = list()

  for (key in names(items)) {
    value = items[[key]]
    if (key %in% names(ordered)) {
      stop(sprintf('ModelError: Duplicate key %s', key))
    }
    ordered[[key]] = value
  }

  # TODO sort list by keys

  ordered
}



read_model_metadata <- function(source) {
  info = file.info(source)
  if (info$isdir) {
    read_model_metadata_bundle(source)
  } else {
    pvt_json_from_url(source)
  }
}

pvt_json_from_url = function(source) {
  jsonlite::fromJSON(readChar(source, file.info(source)$size), simplifyVector = F)
}

read_model_metadata_bundle <- function(path) {
  # """Load logical model a directory specified by `path`.  Returns a model
  #   description dictionary. Model directory bundle has structure:
  #
  # * ``model.cubesmodel/``
  # * ``model.json``
  # * ``dim_*.json``
  # * ``cube_*.json``
  #
  # The dimensions and cubes lists in the ``model.json`` are concatenated with
  # dimensions and cubes from the separate files.

  info = file.info(path)

  if (!info$isdir)
    stop(sprintf("ArgumentError: Path '%s' is not a directory.", path))

  info_path = file.path(path, 'model.json')

  if (!file.exists(info_path))
    stop(sprintf('ModelError: main model info %s does not exist', info_path))

  model = pvt_json_from_url(info_path)

  # Find model object files and load them

  if (is.null(model$dimensions))
    model$dimensions = list()

  if (is.null(model$cubes))
    model$cubes = list()

  for (filename in dir(path, pattern = '*.json')) {
    split = stringi::stri_split_fixed(filename, '_', simplify = T)
    prefix = split[1]
    obj_path = file.path(path, filename)

    desc = pvt_json_from_url(obj_path)

    if (prefix %in% c('dim', 'dimension')) {

      if (is.null(desc$name)) {
        stop(sprintf("ModelError: Dimension file '%s' has no name key", obj_path))
      }
      name = desc$name

      if (name %in% sapply(model$dimensions, function(dim)dim$name)) {
        stop(sprintf("ModelError: Dimension '%s' defined multiple times (in '%s')", name, obj_path))
      }

      model$dimensions[[length(model$dimensions) + 1]] = desc

    } else if(prefix == 'cube') {

      if (is.null(desc$name)) {
        stop(sprintf("ModelError: Cube file '%s' has no name key", obj_path))
      }
      name = desc$name

      if (name %in% sapply(model$cubes, function(cube)cube$name)) {
        stop(sprintf("ModelError: Cube '%s' defined multiple times (in '%s')", name, obj_path))
      }

      model$cubes[[length(model$cubes) + 1]] = desc
    }
  }

  model
}

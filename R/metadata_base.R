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




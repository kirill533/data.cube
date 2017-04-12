compareListOfClasses = function(list1, list2) {
  list1 = as.list(list1)
  list2 = as.list(list2)
  if (length(list1) != length(list2))
    return(FALSE)
  for (i in 1:length(list1))
    if (!list1[[i]]$equal_to(list2[[i]]))
      return(FALSE)

  TRUE
}

PVT_DEFAULT_LEVEL_ROLES = list(
    "time" = c("year", "quarter", "month", "day", "hour", "minute", "second",
    "week", "weeknum", "dow",
    "isoyear", "isoweek", "isoweekday")
)


# Note: levels and hierarchies will be depreciated in the future versions.
# Levels will disappear and hierarchies will be top-level objects.

# TODO: Serves just as reminder for future direction. No real use yet.
Conceptual = setRefClass(
  'Conceptual',
  contains="ModelObject",
  #fields=c(),
  methods=list(
      levels = function() {
          #"""Return list of levels of the conceptual object. Dimension returns
          #        just list of itself, hierarchy returns list of it's dimensions."""
        stop("NotImplementedError: Subclasses sohuld implement levels")
      },

      is_flat = function() {
          stop("NotImplementedError: Subclasses should implement is_flat")
      }
  )
)

Dimension = setRefClass(
  'Dimension',
  contains="Conceptual",
  fields = c('name', 'pvt_levels', 'role', 'cardinality', 'category',
             'master', 'label', 'default_hierarchy_name', 'nonadditive',
             'pvt_attributes_by_ref', 'pvt_attributes', 'pvt_flat_hierarchy',
             'pvt_hierarchies', 'pvt_default_hierarchy',
             'pvt_hierarchy'),
  methods=list(

  # TODO: new signature: __init__(self, name, *attributes, **kwargs):
  initialize = function(name, levels=NULL, hierarchies=NULL,
               default_hierarchy_name=NULL, label=NULL, description=NULL,
               info=NULL, role=NULL, cardinality=NULL, category=NULL,
               master=NULL, nonadditive=NULL, attributes=NULL, ...) {
    # Create a new dimension
    #
    # Attributes:
    #
    # * `name`: dimension name
    # * `levels`: list of dimension levels (see: :class:`cubes.Level`)
    # * `hierarchies`: list of dimension hierarchies. If no hierarchies are
    # specified, then default one is created from ordered list of `levels`.
    # * `default_hierarchy_name`: name of a hierarchy that will be used when
    # no hierarchy is explicitly specified
    # * `label`: dimension name that will be displayed (human readable)
    # * `description`: human readable dimension description
    # * `info` - custom information dictionary, might be used to store
    # application/front-end specific information (icon, color, ...)
    # * `role` – one of recognized special dimension types. Currently
    # supported is only ``time``.
    # * `cardinality` – cardinality of the dimension members. Used
    # optionally by the backends for load protection and frontends for
    # better auto-generated front-ends. See :class:`Level` for more
    # information, as this attribute is inherited by the levels, if not
    # specified explicitly in the level.
    # * `category` – logical dimension group (user-oriented metadata)
    # * `nonadditive` – kind of non-additivity of the dimension. Possible
    # values: `NULL` (fully additive, default), ``time`` (non-additive for
    # time dimensions) or ``all`` (non-additive for any other dimension)
    # * `attributes` – attributes for dimension. Use either this or levels,
    # not both.
    #
    # Dimension class is not meant to be mutable. All level attributes will
    # have new dimension assigned.
    #
    # Note that the dimension will claim ownership of levels and their
    # attributes. You should make sure that you pass a copy of levels if you
    # are cloning another dimension.
    #
    #
    # Note: The hierarchy will be owned by the dimension.

    callSuper(name=name, label=label, description=description, info=info, ...)

    role <<- role
    cardinality <<- cardinality
    category <<- category

    # Master dimension – dimension that this one was derived from, for
    # example by limiting hierarchies
    # TODO: not yet documented
    # TODO: probably replace the limit using limits in-dimension instead
    # of replacement of instance variables with limited content (?)
    master <<- master

    # Note: synchronize with Measure.__init__ if relevant/necessary
    if (is.null(nonadditive) || nonadditive == "NULL" || !nonadditive) {
      nonadditive <<- NULL
    } else if (nonadditive %in% c("all", "any")) {
      nonadditive <<- "all"
    } else if (nonadditive != "time") {
      stop(sprintf("ModelError: Unknown non-additive diension type '%s'", nonadditive))
    } else {
      nonadditive <<- nonadditive
    }

    if (!is.null(levels) && length(levels) == 0)
      levels = NULL


    if (is.null(levels) && is.null(attributes)) {
      stop(sprintf("ModelError: No levels or attriutes specified for dimension %s", name))
    } else if (!is.null(levels) && !is.null(attributes)) {
      stop(sprintf("ModelError: Both levels and attributes specified for dimension %s", name))
    }

    setLevels(levels)

    if (!is.null(attributes)) {
      # TODO: pass all level initialization arguments here
      level = list(Level$new(name, attributes=attributes))
      setLevels(level)
    }

    # Own the levels and their attributes
    if (!is.null(role)) {
      default_roles = PVT_DEFAULT_LEVEL_ROLES[[nvl(role, 1)]]
    } else {
      default_roles = NULL
    }

    new_levels = getLevels()
    # Set default roles
    for (level_key in names(new_levels)) {
      if (!is.null(default_roles) && !is.null(default_roles[[level$name]])) {
        new_levels[[level_key]]$role = new_levels[[level_key]]$name
      }
    }
    setLevels(new_levels)

    # Collect attributes
    pvt_attributes <<- list() # OrderedDict
    pvt_attributes_by_ref <<- list() # OrderedDict

    for (level in getLevels()) {
      for (a in level$attributes) {
        # Own the attribute
        if (!is.null(a$dimension) && a$dimension$name != name) {
          stop(sprintf("ModelError: Dimension '%s' can not claim attribute '%s' because it is owned by another dimension '%s'.",
                       name, a$name, a$dimension$name))
        }

        a$setDimension(.self)
        pvt_attributes[[a$name]] <<- a
        pvt_attributes_by_ref[[a$ref]] <<- a
      }
    }


    # The hierarchies receive levels with already owned attributes
    if (!is.null(hierarchies)) {
      # error_message = "Duplicate hierarchy '{key}' in cube '{cube}'"

      # error_dict = {"cube": self.name}

      pvt_hierarchies <<- object_dict(hierarchies)
    } else {
      default = Hierarchy$new("default", getLevels())
      pvt_hierarchies <<- object_dict(list(default))
    }

    pvt_flat_hierarchy <<- NULL

    # Set default hierarchy specified by ``default_hierarchy_name``, if
    # the variable is not set then get a hierarchy with name *default* or
    # the first hierarchy in the hierarchy list.

    default_name = nvl(default_hierarchy_name, "default")
    hierarchy = nvl(pvt_hierarchies[['default_name']], pvt_hierarchies[[1]])

    pvt_default_hierarchy <<- hierarchy
    default_hierarchy_name <<- hierarchy$name

  },

  equal_to = function(other) {
    if (is.null(other) || class(other)[1] != class(.self)[1]) {
      return(FALSE)
    }

    cond = name == other$name &&
      nvl(role, '') == nvl(other$role, '') &&
      nvl(label, '') == nvl(other$label, '') &&
      nvl(description, '') == nvl(other$description, '') &&
      nvl(cardinality, '') == nvl(other$cardinality, '') &&
      nvl(category, '') == nvl(other$category, '') &&
      nvl(default_hierarchy_name, '') == nvl(other$default_hierarchy_name, '')
      # pvt_levels == other$pvt_levels && TODO finish comparison of lists (after refactoring to not have recursive references)
      # pvt_hierarchies == other$pvt_hierarchies TODO finish comparison of lists (after refactoring to not have recursive references)

    cond
  },

  copy = function(shallow = FALSE) {
    def <- .refClassDef
    value <- new(def, name = .self$name, label = .self$label,
                 description = .self$description,
                 info = .self$info, levels = getLevels())
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
  },

  has_details = function() {
    # Returns ``True`` when each level has only one attribute, usually key.

    if (!is.null(master)) {
      master$has_details()
    } else {
      any(sapply(getLevels(), function(level) {level$has_details()}))
    }
  },

  getLevels = function() {
    # Get list of all dimension levels. Order is not guaranteed, use a
    # hierarchy to have known order.
    nl = pvt_levels
    names(nl) <- NULL
    nl
  },

  setLevels = function(levels) {
    pvt_levels <<- list()
    for (level in levels) {
      pvt_levels[[level$name]] <<- level
    }
  },

  setHierarchies = function(hierarchies) {
    pvt_hierarchies <<- list()
    for (hier in hierarchies)
      pvt_hierarchies[[hier.name]] <<- hier
  },

  hierarchies = function() {
    # """Get list of dimension hierarchies."""

    nl = pvt_hierarchies
    names(nl) <- NULL
    nl
  },

  level_names = function(self) {
    # """Get list of level names. Order is not guaranteed, use a hierarchy
    #     to have known order."""

    names(pvt_levels)
  },

  level = function(obj) {
    # """Get level by name or as Level object. This method is used for
    #     coalescing value"""

    if (is.character(obj)) {
      if (is.null(pvt_levels[[obj]])) {
        stop(sprintf("KeyError: No level %s in dimension %s", obj, name))
      }

      pvt_levels[[obj]]
    } else if ('Level' %in% class(obj)) {
      obj
    } else {
      stop(sprintf("ValueError: Unknown level object %s (should be a string or Level)", typeof(obj)))
    }
  },

  hierarchy = function(obj=NULL) {
    # """Get hierarchy object either by name or as `Hierarchy`. If `obj` is
    #     ``None`` then default hierarchy is returned."""

    if (is.null(obj)) {
      pvt_default_hierarchy
    } else if (is.character(obj)) {
      if (is.null(pvt_hierarchies[[obj]])) {
        stop(sprintf("ModelError: No hierarchy %s in dimension %s", obj, name))
      }
      pvt_hierarchies[[obj]]
    } else if ('Hierarchy' %in% class(obj)) {
      obj
    } else {
      stop(sprintf("ValueError: Unknown hierarchy object %s (should be a string or Hierarchy instance)", typeof(obj)))
    }
  },

  attribute = function(name, by_ref=FALSE, throw_error = TRUE) {
    # Get dimension attribute. `name` is an attribute name (default) or
    # attribute reference if `by_ref` is `True`.`.
    res = NULL
    if (by_ref) {
      res = pvt_attributes_by_ref[[name]]
    } else {
      res = pvt_attributes[[name]]

      if (is.null(res) && throw_error) {
        stop(sprintf("NoSuchAttributeError: Unknown attribute '{}' in dimension '{}'", name, .self$name))
      }
    }

    return(res)
  },

  is_flat = function() {
    # Is true if dimension has only one level

    if (!is.null(master)) {
      master$is_flat()
    } else {
      length(pvt_levels) == 1
    }
  },

  key_attributes = function() {
    # """Return all dimension key attributes, regardless of hierarchy. Order
    #     is not guaranteed, use a hierarchy to have known order."""

    sapply(pvt_levels, function(level){level$key})
  },

  attributes = function() {
    # """Return all dimension attributes regardless of hierarchy. Order is
    #     not guaranteed, use :meth:`cubes.Hierarchy.all_attributes` to get
    #     known order. Order of attributes within level is preserved."""


    nl = pvt_attributes
    names(nl) <- NULL
    nl
  },

  # @deprecated use copy() instead
  clone = function(name = NULL, hierarchies=NULL, exclude_hierarchies=NULL,
            nonadditive=NULL, default_hierarchy_name=NULL, cardinality=NULL,
            alias=NULL, ...) {
    # Returns a clone of the receiver with some modifications. `master`
    # of the clone is set to the receiver.
    #
    # * `hierarchies` – limit hierarchies only to those specified in
    # `hierarchies`. If default hierarchy name is not in the new hierarchy
    # list, then the first hierarchy from the list is used.
    # * `exclude_hierarchies` – all hierarchies are preserved except the
    # hierarchies in this list
    # * `nonadditive` – non-additive value for the dimension
    # * `alias` – name of the cloned dimension

    if (!is.null(hierarchies) && length(hierarchies) == 0) {
      stop(sprintf("ModelInconsistencyError: Can not remove all hierarchies from a dimension (%s).", name))
    }

    if (!is.null(hierarchies)) {
      linked = c()
      for (name in hierarchies) {
        linked = append(linked, hierarchy(name))
      }
    } else if (!is.null(exclude_hierarchies) && exclude_hierarchies == TRUE) {
      linked = c()
      for (hierarchy in pvt_hierarchies) {
        if (!(hierarchy$name %in% exclude_hierarchies)) {
          linked = append(linked, hierarchy)
        }
      }
    } else {
      linked = pvt_hierarchies
    }

    if (length(linked) == 0)
      stop("ModelError: No hierarchies to clone.")

    hierarchies = lapply(linked, function(hier){hier$copy()}) # $copy() does not work properly with mandatory fields

    # Get relevant levels
    levels = c()
    seen = c()

    # Get only levels used in the hierarchies
    for (hier in hierarchies) {
      for (level in hier$getLevels()) {
        if (!(level$name %in% seen)) {
          levels = append(levels, level)
          seen = append(seen, level$name)
        }
      }
    }


    # Dis-own the level attributes (we already have a copy)
    for (level in levels) {
      for (attribute in level$attributes) {
        attribute$dimension = NULL
      }
    }

    loc_nonadditive = nvl(nonadditive, .self$nonadditive)
    loc_cardinality = nvl(cardinality, .self$cardinality)

    # We are not checking whether the default hierarchy name provided is
    # valid here, as it was specified explicitly with user's knowledge and
    # we might fail later. However, we need to check the existing default
    # hierarchy name and replace it with first available hierarchy if it
    # is invalid.

    def_hier = nvl(default_hierarchy_name, .self$default_hierarchy_name)

    if (!any(sapply(hierarchies, function(hier){hier$name == def_hier}))) {
      def_hier = hierarchies[[1]]$name
    }

    # TODO: should we do deppcopy on info?
    loc_name = nvl(alias, .self$name)

    Dimension$new(name=loc_name,
                     levels=levels,
                     hierarchies=hierarchies,
                     default_hierarchy_name=default_hierarchy_name,
                     label=label,
                     description=description,
                     info=info,
                     role=role,
                     cardinality=loc_cardinality,
                     master=.self,
                     nonadditive=loc_nonadditive,
                     ...)

  },

  to_dict = function(options = list()) {
    # """Return dictionary representation of the dimension"""

    out = callSuper(options = options)

    hierarchy_limits = options$hierarchy_limits

    out$default_hierarchy_name = hierarchy()$name

    out$role = role
    out$cardinality = cardinality
    out$category = category

    out$levels = lapply(getLevels(), function(level){level$to_dict(options)})

    # Collect hierarchies and apply hierarchy depth restrictions
    hierarchies = c()
    hierarchy_limits = nvl(hierarchy_limits, list())
    for (hname in names(pvt_hierarchies)) {
      h = pvt_hierarchies[[hname]]
      if (!is.null(hierarchy_limits[[hname]])) {
        level = hierarchy_limits[[hname]]
        if (level) {
          depth = h$level_index(level) + 1
          options_h = options
          options_h$depth = depth
          restricted = h$to_dict(options_h)
          hierarchies = append(hierarchies, restricted)
        } else {
          # we ignore the hierarchy
        }
      } else {
        append(hierarchies, h$to_dict(options))
      }
    }

    out$hierarchies = hierarchies

    # Use only for reading, during initialization these keys are ignored,
    # as they are derived
    # They are provided here for convenience.
    out$is_flat = is_flat()
    out$has_details = has_details()

    out
  }


))
#    Cube dimension.



Dimension.from_metadata <- function(metadata, templates=NULL) {
#        Create a dimension from a `metadata` dictionary.  Some rules:
#
#        * ``levels`` might contain level names as strings – names of levels to
#          inherit from the template
#        * ``hierarchies`` might contain hierarchies as strings – names of
#          hierarchies to inherit from the template
#        * all levels that are not covered by hierarchies are not included in the
#          final dimension

  templates = nvl(templates, list())

  if (!is.character(metadata) && !is.null(metadata$template)) {

    template_name = metadata$template

    if (!is.null(templates[[template_name]]))
      template = templates[[template_name]]
    else
      stop(sprintf('TemplateRequired %s', template_name))

    levels = lapply(template$getLevels(), function(level){level$copy()})

    # Create copy of template's hierarchies, but reference newly
    # created copies of level objects
    hierarchies = c()
    level_dict = object_dict(levels)

    for (hier in template$hierarchies()) {
      hier_levels = level_dict[as.character(sapply(hier$getLevels(), function(level){level$name}))]
      hier_copy = Hierarchy(hier$name,
                            hier_levels,
                            label=hier$label,
                            info=hier$info) # copy.deepcopy
      hierarchies = append(hierarchies, hier_copy)
    }

    default_hierarchy_name = template$default_hierarchy_name
    label = template$label
    description = template$description
    info = template$info
    cardinality = template$cardinality
    role = template$role
    category = template$category
    nonadditive = template$nonadditive

  } else {
    template = NULL
    levels = c()
    hierarchies = c()
    default_hierarchy_name = NULL
    label = NULL
    description = NULL
    cardinality = NULL
    role = NULL
    category = NULL
    info = list()
    nonadditive = NULL
  }


  # Fix the metadata, but don't create default level if the template
  # provides levels.
  metadata = expand_dimension_metadata(metadata,
                                       expand_levels=length(levels) < 1)

  name = metadata$name

  label = nvl(metadata$label, label)
  description = nvl(metadata$description, description)
  info = nvl(metadata$info, info)
  role = nvl(metadata$role, role)
  category = nvl(metadata$category, category)
  nonadditive = nvl(metadata$nonadditive, nonadditive)

  # Levels
  # ------

  # We are guaranteed to have "levels" key from expand_dimension_metadata()

  if (!is.null(metadata$levels)) {
    # Assure level inheritance
    levels = c()
    skip = FALSE

    for (level_md in metadata$levels) {
      if (is.character(level_md)) {
        if (!template) {
          stop(sprintf("Can not specify just a level name (%s) if there is no template for dimension %s", level_md, name))
        }
        level = template$level(level_md)
      } else {
        level = Level.from_metadata(level_md)
      }

      # Update the level's info dictionary
      if (!is.null(template)) {
        tryCatch({
          templevel = template$level(level$name)
        }, error = function(e){
          skip = TRUE
        })

        if (!skip) {
          new_info = templevel$info # was deep copy
          new_info = listMerge(new_info, level$info)
          level$info = new_info
        }

      }

      if (!skip)
        levels = c(levels, level)
    }
  }

  level_names = sapply(levels, function(level){level$name})

  # Hierarchies
  # -----------
  if (!is.null(metadata$hierarchies)) {
    hierarchies = pvt_create_hierarchies(metadata$hierarchies,
                                      levels,
                                      template)
  } else {
    # Keep only hierarchies which include existing levels

    keep = sapply(hierarchies, function(hier){all(hier$level_names() %in% level_names)})

    hierarchies = c(hierarchies[keep])
  }

  default_hierarchy_name = nvl(metadata$default_hierarchy_name, default_hierarchy_name)

  if (length(hierarchies) == 0) {
    # Create single default hierarchy
    hierarchies = c(Hierarchy$new("default", levels=levels))
  }

  # Recollect levels – keep only those levels that are present in
  # hierarchies. Retain the original level order
  used_levels = c()
  for(hier in hierarchies) {
    used_levels = c(used_levels, sapply(hier$getLevels(), function(level)level$name))
  }
  used_levels = unique(used_levels)

  levels = levels[level_names %in% used_levels]

  Dimension$new(name=name,
             levels=levels,
             hierarchies=hierarchies,
             default_hierarchy_name=default_hierarchy_name,
             label=label,
             description=description,
             info=info,
             cardinality=cardinality,
             role=role,
             category=category,
             nonadditive=nonadditive
  )

}

as.character.Dimension = function(dim, mode = "all") {
  dim$name
}

as.vector.Dimension = function(dim, mode = "all") {
  dim$name
}

pvt_create_hierarchies <- function(metadata, levels, template=NULL) {
  # Create dimension hierarchies from `metadata` (a list of dictionaries or
  # strings) and possibly inherit from `template` dimension.

  # Convert levels do an ordered dictionary for access by name
  levels = object_dict(levels)
  hierarchies = c()

  # Construct hierarchies and assign actual level objects
  if (!is.null(names(metadata))) {
    metadata = list(metadata)
  }
  for (md in metadata) {
    if (is.character(md)) {
      if (is.null(template)) {
        stop(sprintf("ModelError: Can not specify just a hierarchy name (%s) if there is no template", md))
      }
      hier = template$hierarchy(md)
    } else {
      md = as.list(md)
      level_names = md$levels
      hier_levels = levels[unlist(level_names)]
      md$levels=hier_levels
      hier = do.call(Hierarchy$new, md)
    }

    hierarchies = append(hierarchies, hier)
  }


  hierarchies
}

Hierarchy = setRefClass(
  'Hierarchy',
  contains = 'Conceptual',
  fields = c('pvt_levels'),
  methods = list(
    initialize = function(name, levels, label=NULL, description=NULL, info=NULL, ...) {
      # """Dimension hierarchy - specifies order of dimension levels.
      #
      #   Attributes:
      #
      #   * `name`: hierarchy name
      #   * `levels`: ordered list of levels or level names from `dimension`
      #
      #   * `label`: human readable name
      #   * `description`: user description of the hierarchy
      #   * `info` - custom information dictionary, might be used to store
      #     application/front-end specific information
      #
      #   Some collection operations might be used, such as ``level in hierarchy``
      #   or ``hierarchy[index]``. String value ``str(hierarchy)`` gives the
      #   hierarchy name.
      #
      #   Note: The `levels` should have attributes already owned by a
      #   dimension.
      #   """

      callSuper(name=name, label=label, description=description, info=info, ...)

      if (is.null(levels) || length(levels) == 0) {
        stop(sprintf("ModelInconsistencyError: Hierarchy level list should not be empty (in %s)", name))
      }

      if (any(is.character(levels)) || any(sapply(levels, is.character))) {
        stop("ModelInconsistencyError: Levels should not be provided as strings to Hierarchy.")
      }

      pvt_levels <<- object_dict(levels)
    },

    copy = function(shallow = FALSE) {
      def <- .refClassDef
      value <- new(def, name = .self$name, label = .self$label,
                   description = .self$description,
                   info = .self$info, levels = sapply(getLevels(), function(l){l$copy(shallow)}))
      vEnv <- as.environment(value)
      selfEnv <- as.environment(.self)
      for (field in names(def@fieldClasses)) {
        if (field == 'pvt_levels') {
          # do nothing
        } else if (shallow) {
          assign(field, get(field, envir = selfEnv), envir = vEnv)
        } else {
          current <- get(field, envir = selfEnv)
          if (is(current, "envRefClass"))
            current <- current$copy(FALSE)
          assign(field, current, envir = vEnv)
        }
      }
      value
    },

    getLevels = function() {

      nl = pvt_levels
      names(nl) <- NULL
      nl
    },

    setLevels = function(levels) {
      pvt_levels <<- list()

      for (level in levels) {
        pvt_levels[[level$name]] <<- level
      }
    },

    level_names = function() {
      names(pvt_levels)
    },

    equal_to = function(other) {
      if (is.null(other) || class(other)[1] != class(.self)[1])
        return(FALSE)

      name == other$name && nvl(label, '') == nvl(other$label, '') && compareListOfClasses(getLevels(), other$getLevels())
    },

    len = function() {
      length(pvt_levels)
    },

    getitem = function(item) {

      if ((is.numeric(item) && (item < 1 || item > length(pvt_levels)) ) || is.null(pvt_levels[[item]])) {
        stop(sprintf("HierarchyError: Hierarchy '%s' has only %d levels, asking for deeper level",
                     name, length(pvt_levels) ))
      }
      pvt_levels[[item]]
    },

    contains = function(item) {

      if (is.character(item)) {
        item.name = item
      } else {
        item.name = item$name
      }

      level = pvt_levels[[item.name]]

      if (is.null(level)) {
        FALSE
      } else if(!is.character(item)) {
        level$equal_to(item)
      } else {
        TRUE
      }
    },

    levels_for_path = function(path, drilldown=FALSE) {
      # """Returns levels for given path. If path is longer than hierarchy
      # levels, `cubes.ArgumentError` exception is raised"""

      depth = ifelse(is.null(path), 0, length(path))

      levels_for_depth(depth, drilldown)
    },

    levels_for_depth = function(depth, drilldown=FALSE) {
      # """Returns levels for given `depth`. If `path` is longer than
      # hierarchy levels, `cubes.ArgumentError` exception is raised"""

      if (depth == 0) {
        return(list())
      }
      depth = nvl(depth, 1)
      extend = ifelse(drilldown, 1, 0)

      if (depth + extend > length(getLevels())) {
        stop(sprintf("HierarchyError: Depth %d is longer than hierarchy levels (drilldown: %s)", depth, drilldown))
      }

      l = getLevels()
      # if (depth + extend == 1) {
      #   l[[1]]
      # } else {
        l[1:(depth + extend)]
      # }
    },

    next_level = function(level) {
      # """Returns next level in hierarchy after `level`. If `level` is last
      # level, returns ``None``. If `level` is ``None``, then the first level
      # is returned."""

      if (is.null(level)) {
        return(getLevels()[[1]])
      }


      l = getLevels()

      return_next = FALSE
      for (l_item in l) {
        if (return_next) {
          return(l_item)
        }
        if (l_item$equal_to(level)) {
          return_next = TRUE
        }
      }

      NULL
    },

    previous_level = function(level) {
      # """Returns previous level in hierarchy after `level`. If `level` is
      # first level or ``None``, returns ``None``"""

      if (is.null(level)) {
        return(NULL)
      }

      l = getLevels()

      prev = NULL
      for (l_item in l) {
        if (l_item$equal_to(level)) {
          return(prev)
        }
        prev = l_item
      }

      NULL
    },

    level_index = function(level) {
      # """Get order index of level. Can be used for ordering and comparing
      # levels within hierarchy."""

      if (!is.null(level)) {
        l = getLevels()

        index = 0
        for (l_item in l) {
          index = index + 1
          if (is.character(level)) {
            if (level == as.character(l_item)) {
              return(index)
            }
          } else if (l_item$equal_to(level)) {
            return(index)
          }
        }
      }

      stop(sprintf("HierarchyError: Level %s is not part of hierarchy %s", as.character(level), name))
    },

    is_last = function(level) {
      # """Returns `True` if `level` is last level of the hierarchy."""
      l = getLevels()

      l[[length(l)]]$equal_to(level)
    },

    rollup = function(path, level=NULL) {
      # """Rolls-up the path to the `level`. If `level` is ``None`` then path
      # is rolled-up only one level.
      # If `level` is deeper than last level of `path` the
      # `cubes.HierarchyError` exception is raised. If `level` is the same as
      # `path` level, nothing happens."""

      if (!is.null(level)) {
        last = level_index(level)
        if (last > length(path)) {
          stop(sprintf("HierarchyError: Can not roll-up: level '%s' – it is deeper than deepest element of path %s", as.character(level), paste0(path, collapse = ',')))
        }
      } else if (length(path) > 0) {
        last = length(path) - 1
      } else {
        last = NULL
      }

      if (is.null(last)) {
        return(list())
      } else {
        path[1:last]
      }

    },

    path_is_base = function(path) {
      # """Returns True if path is base path for the hierarchy. Base path is a
      # path where there are no more levels to be added - no drill down
      # possible."""

      !is.null(path) && length(path) == length(getLevels())
    },

    key_attributes = function() {
      # """Return all dimension key attributes as a single list."""

      sapply(getLevels(), function(level){level$key})
    },

    all_attributes = function() {
      # """Return all dimension attributes as a single list."""

      attributes = c()
      for (level in getLevels())
        attributes = append(attributes, level$attributes)

      attributes
    },

    to_dict = function(options = list()) {
      # """Convert to dictionary. Keys:
      #
      #   * `name`: hierarchy name
      #   * `label`: human readable label (localizable)
      #   * `levels`: level names
      #
      #   """
      depth = options$depth

      out = callSuper(options)

      levels = as.character(sapply(getLevels(), as.character))

      if (!is.null(depth)) {
        out$levels = levels[1:depth]
      } else {
        out$levels = levels
      }

      out$info = info

      out
    }
  )
)


# Object representing a hierarchy level. Holds all level attributes.
#
# This object is immutable, except localization. You have to set up all
# attributes in the initialisation process.
#
# Attributes:
#
# * `name`: level name
# * `attributes`: list of all level attributes. Raises `ModelError` when
# `attribute` list is empty.
# * `key`: name of level key attribute (for example: ``customer_number`` for
# customer level, ``region_code`` for region level, ``month`` for month
# level).  key will be used as a grouping field for aggregations. Key
# should be unique within level. If not specified, then the first
# attribute is used as key.
# * `order`: ordering of the level. `asc` for ascending, `desc` for
# descending or might be unspecified.
# * `order_attribute`: name of attribute that is going to be used for
# sorting, default is first attribute (usually key)
# * `label_attribute`: name of attribute containing label to be displayed
# (for example: ``customer_name`` for customer level, ``region_name`` for
# region level, ``month_name`` for month level)
# * `label`: human readable label of the level
# * `role`: role of the level within a special dimension
# * `info`: custom information dictionary, might be used to store
# application/front-end specific information
# * `cardinality` – approximation of the number of level's members. Used
#       optionally by backends and front ends.
#     * `nonadditive` – kind of non-additivity of the level. Possible
#       values: `NULL` (fully additive, default), ``time`` (non-additive for
#       time dimensions) or ``all`` (non-additive for any other dimension)
#
#     Cardinality values:
#
#     * ``tiny`` – few values, each value can have it's representation on the
# screen, recommended: up to 5.
# * ``low`` – can be used in a list UI element, recommended 5 to 50 (if sorted)
# * ``medium`` – UI element is a search/text field, recommended for more than 50
# elements
# * ``high`` – backends might refuse to yield results without explicit
# pagination or cut through this level.
#
# Note: the `attributes` are going to be owned by the `dimension`.
Level = setRefClass(
  'Level',
  contains = 'ModelObject',
  fields = c('cardinality', 'role', 'attributes', 'nonadditive', 'key',
             'label_attribute', 'order_attribute', 'order'),
  methods = list(
    initialize = function(name, attributes = NULL, key=NULL, order_attribute=NULL,
                          order=NULL, label_attribute=NULL, label=NULL, info=NULL,
                          cardinality=NULL, role=NULL, nonadditive=NULL,
                          description=NULL) {

      callSuper(name=name, label=label, description=description, info=info)

      cardinality <<- cardinality
      role <<- role

      if (is.null(attributes)) {
        stop('ModelError: Attribute list should not be empty')
      }

      attributes <<- attributes

      # Note: synchronize with Measure.__init__ if relevant/necessary

      if (is.null(nonadditive) || nonadditive == "NULL" || !nonadditive) {
        nonadditive <<- NULL
      } else if(nonadditive %in% c('all', 'any')) {
        nonadditive <<- "all"
      } else if (nonadditive != 'time') {
        stop(sprintf("ModelError: Unknown non-additive diension type '%s'", nonadditive))
      } else {
        nonadditive <<- nonadditive
      }

      if (!is.null(key)) {
        key <<- attribute(key)
      } else if(length(attributes) >= 1) {
        key <<- attributes[[1]]
      } else {
        stop("ModelInconsistencyError: Attribute list should not be empty")
      }

      # Set second attribute to be label attribute if label attribute is not
      # set. If dimension is flat (only one attribute), then use the only
      # key attribute as label.

      if (!is.null(label_attribute)) {
        label_attribute <<- attribute(label_attribute)
      } else if (length(attributes) > 1) {
        label_attribute <<- attributes[[2]]
      } else {
        label_attribute <<- .self$key
      }

      # Set first attribute to be order attribute if order attribute is not set

      if (!is.null(order_attribute)) {
        order_attribute <<- attribute(order_attribute)
      } else {
        order_attribute <<- attributes[[1]]
      }

      order <<- order

      cardinality <<- cardinality

    },

    to_dict = function(options) {
      # """Convert to dictionary"""

      full_attribute_names = nvl(options$full_attribute_names, FALSE)

      out = callSuper(options)

      out$role = role

      if (full_attribute_names) {
        out$key = key$ref
        out$label_attribute = label_attribute$ref
        out$order_attribute = order_attribute$ref
      } else {
        out$key = key$name
        out$label_attribute = label_attribute$name
        out$order_attribute = order_attribute$name
      }

      out$order = order
      out$cardinality = cardinality
      out$nonadditive = nonadditive

      out$attributes = lapply(attributes, function(attr){attr$to_dict(options)})
      out
    },

    # Get attribute by `name`
    attribute = function(name) {

      res = NULL
      for (attr in attributes) {
        if (attr$name == name) {
          res = attr
        }
      }

      if (is.null(res)) {
        stop(sprintf('NoSuchAttributeError %s', name))
      }

      res
    },

    equal_to = function(other) {
      if (is.null(other) || class(other)[1] != class(.self)[1]) {
        FALSE
      } else if (
        name != other$name ||
        nvl(label, '') != nvl(other$label, '') ||
        !key$equal_to(other$key) ||
        nvl(cardinality, '') != nvl(other$cardinality, '') ||
        nvl(role, '') != nvl(other$role, '') ||
        !label_attribute$equal_to(other$label_attribute) ||
        !order_attribute$equal_to(other$order_attribute) ||
        !compareListOfClasses(attributes, other$attributes) ||
        nvl(nonadditive, '') != nvl(other$nonadditive, '')) {

        FALSE
      } else {
        TRUE
      }
    },

    has_details = function() {
      # Is ``True`` when level has more than one attribute, for all levels
      # with only one attribute it is ``False``.

      length(attributes) > 1
    },

    copy = function(shallow = FALSE) {
      def <- .refClassDef
      value <- new(def, name = .self$name, label = .self$label,
                   description = .self$description,
                   info = .self$info, attributes = lapply(.self$attributes, function(a){a$copy()}))
      vEnv <- as.environment(value)
      selfEnv <- as.environment(.self)
      for (field in names(def@fieldClasses)) {
        if (field == 'attributes') {
          # do nothing
        } else if (shallow) {
          assign(field, get(field, envir = selfEnv), envir = vEnv)
        } else {
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

Level.from_metadata <- function(metadata, name=NULL, dimension=NULL) {
  # Create a level object from metadata. `name` can override level name in
  # the metadata.

  metadata = expand_level_metadata(metadata)

  if (is.null(metadata$name)) {
    stop("ModelError: No name specified in level metadata")
  }

  # R specific logic
  attrs = metadata$attributes
  if (!is.null(names(attrs))) {
    attrs = list(attrs)
  }

  attributes = lapply(attrs, function(attr_metadata){
    attr_metadata$dimension = dimension
    Attribute.from_metadata(attr_metadata)
  })

  metadata$name = nvl(name, metadata$name)
  metadata$attributes = attributes

  do.call(Level$new, metadata)
}

as.vector.Level = function(level, mode = "all") {
  level$name
}

expand_dimension_metadata <- function(metadata, expand_levels=F) {
  # Expands `metadata` to be as complete as possible dimension metadata. If
  # `expand_levels` is `True` then levels metadata are expanded as well.

  if (is.character(metadata)) {
    metadata = list("name" = metadata, "levels" = metadata)
  } else {
    if (is.null(metadata$name)) {
      stop("ModelError: Dimension has no name")
    }
  }

  name = metadata$name

  # Fix levels
  levels = nvl(metadata$levels, list())

  if (!is.null(names(levels))) {
    levels = list(levels)
  }

  if (length(levels) < 1 && expand_levels) {
    attributes = c("attributes", "key", "order_attribute", "order", "label_attribute")

    level = list()
    for (attr in attributes) {
      if (!is.null(metadata[[attr]])) {
        level[[attr]] = metadata[[attr]]
      }
    }

    level$cardinality = metadata$cardinality

    # Default: if no attributes, then there is single flat attribute
    # whith same name as the dimension
    level$name = name
    level$label = metadata$label

    levels = list(level)
  }

  if (length(levels) > 0) {
    levels = lapply(levels, expand_level_metadata)
    metadata$levels = levels
  }

  # Fix hierarchies
  if (!is.null(metadata$hierarchy) && !is.null(metadata$hierarchies)) {
    stop("ModelInconsistencyError: Both 'hierarchy' and 'hierarchies' specified. Use only one")
  }

  hierarchy = metadata$hierarchy

  if (!is.null(hierarchy)) {
    hierarchies = list("name" = "default", "levels" = hierarchy)
  } else {
    hierarchies = metadata$hierarchies
  }

  if (!is.null(hierarchies)) {
    metadata$hierarchies = hierarchies
  }

  metadata
}


expand_hierarchy_metadata <- function(metadata) {
  # Returns a hierarchy metadata as a dictionary. Makes sure that required
  # properties are present. Raises exception on missing values.

  if (is.null(metadata$name)) {
    stop("ModelError: Hierarchy has no name")
  }

  if (is.null(metadata$levels)) {
    stop(sprintf("ModelError: Hierarchy '%s' has no levels", metadata$name))
  }

  metadata
}


expand_level_metadata <- function(metadata) {
  # Returns a level description as a dictionary. If provided as string,
  # then it is going to be used as level name and as its only attribute. If a
  # dictionary is provided and has no attributes, then level will contain only
  # attribute with the same name as the level name.

  if (is.character(metadata)) {
    metadata = list("name" = metadata, "attributes" = metadata)
  }
  if (is.null(metadata$name)) {
    stop("ModelError: Level has no name")
  }
  name = metadata$name

  attributes = metadata$attributes

  if (is.null(attributes)) {
    attribute = list(
      "name" = name,
      "label" = metadata$label
    )
    attributes = list(attribute)
  }

  # TODO: this should belong to attributes.py
  metadata$attributes = lapply(attributes, expand_attribute_metadata)

  metadata
}


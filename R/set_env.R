debug <- FALSE

# Set Constructor ####

#' Title Set Environment
#'
#' @return A new set_env object.
#' @export set_env
#'
#' @examples
#' mySet <- set_env()
set_env <- function() {
  env <- new.env(parent = emptyenv(), hash = TRUE)
  class(env) <- "set_env"
  env
}

# Add Method ####
#' Title Add Object to Set
#'
#' @param set The set_env to which to add the object.
#' @param object The object to add to the set.
#'
#' @return The set_env with the object added.
#' @export add.set_env
#'
#' @examples
#' set <- set_env()
#' obj <- "apple"
#' set <- add.set_env(set, obj)
add.set_env <- function(set, object) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  # Generate a hash as a unique identifier
  object_id <- digest::digest(object, algo = "md5")
  assign(object_id, object, envir = set)
  return(set)
}

# Remove Method ####
#' Title Remove Object from Set
#'
#' @param set The set_env from which to remove the object.
#' @param object The object to remove from the set.
#'
#' @return The set_env with the object removed.
#' @export remove.set_env
#'
#' @examples
#' set <- set_env()
#' obj <- "apple"
#' set <- add.set_env(set, obj)
#' set <- remove.set_env(set, obj)
remove.set_env <- function(set, object) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  # Generate the hash for the object to be removed
  object_id <- digest::digest(object, algo = "md5")
  if (exists(object_id, envir = set)) {
    rm(list = object_id, envir = set)
    #message("Object removed successfully.")
  } #else {
    #message("Object not found in the set.")
  #}
  return(set)
}

# Remove All Method ####
#' Title Remove All Objects from Set
#'
#' @param set The set_env from which to remove all objects.
#'
#' @return The set_env with all objects removed.
#' @export remove_all.set_env
#'
#' @examples
#' set <- set_env()
#' set <- add_many.set_env(set, c("apple", "banana"))
#' set <- remove_all.set_env(set)
remove_all.set_env <- function(set) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  # List all objects in the set
  objects <- ls(envir = set)
  # Remove all objects
  if (length(objects) > 0) {
    rm(list = objects, envir = set)
    #message("All objects removed successfully.")
  } #else {
    #message("The set is already empty.")
  #}
  return(set)
}

#Union####
#' Title Union of Two Sets
#'
#' @param set1 First set to combine.
#' @param set2 Second set to combine.
#'
#' @return A new set_env object containing the union of the two sets.
#' @export union.set_env
#'
#' @examples
#' set1 <- set_env()
#' set1 <- add.set_env(set1, "apple")
#' set2 <- set_env()
#' set2 <- add.set_env(set2, "banana")
#' unionSet <- union.set_env(set1, set2)
union.set_env <- function(set1, set2) {
  result_set <- set_env()
  for (object in get_objects.set_env(set1)) {
    result_set <- add.set_env(result_set, object)
  }
  for (object in get_objects.set_env(set2)) {
    if (!contains.set_env(result_set, object)) {
      result_set <- add.set_env(result_set, object)
    }
  }
  return(result_set)
}

#Intersection####
#' Title Intersection of Two Sets
#'
#' @param set1 First set to compare.
#' @param set2 Second set to compare.
#'
#' @return A new set_env object containing the intersection of the two sets.
#' @export intersection.set_env
#'
#' @examples
#' set1 <- set_env()
#' set1 <- add.set_env(set1, "apple")
#' set1 <- add.set_env(set1, "banana")
#' set2 <- set_env()
#' set2 <- add.set_env(set2, "banana")
#' intersectionSet <- intersection.set_env(set1, set2)
intersection.set_env <- function(set1, set2) {
  result_set <- set_env()
  for (object in get_objects.set_env(set1)) {
    if (contains.set_env(set2, object)) {
      result_set <- add.set_env(result_set, object)
    }
  }
  return(result_set)
}

#Difference ####
#' Title Difference of Two Sets
#'
#' @param set1 First set to compare.
#' @param set2 Second set to compare.
#'
#' @return A new set_env object containing the difference of the two sets.
#' @export difference.set_env
#'
#' @examples
#' set1 <- set_env()
#' set1 <- add.set_env(set1, "apple")
#' set1 <- add.set_env(set1, "banana")
#' set2 <- set_env()
#' set2 <- add.set_env(set2, "banana")
#' differenceSet <- difference.set_env(set1, set2)
difference.set_env <- function(set1, set2) {
  result_set <- set_env()
  for (object in get_objects.set_env(set1)) {
    if (!contains.set_env(set2, object)) {
      result_set <- add.set_env(result_set, object)
    }
  }
  return(result_set)
}

# Contains Method####
#' Title Check if Object is in Set
#'
#' @param set The set_env to check.
#' @param object The object to check for.
#'
#' @return TRUE if the object is in the set, FALSE otherwise.
#' @export contains.set_env
#'
#' @examples
#' set <- set_env()
#' obj <- "apple"
#' set <- add.set_env(set, obj)
#' contains.set_env(set, obj) # Should return TRUE
contains.set_env <- function(set, object) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  # Generate the hash for the object
  object_id <- digest::digest(object, algo = "md5")
  # Check if an object with this hash exists in the set
  return(exists(object_id, envir = set))
}

# Get Elements Method####
#' Title Get Objects in Set
#'
#' @param set The set_env to retrieve objects from.
#'
#' @return A list of objects in the set.
#' @export get_objects.set_env
#'
#' @examples
#' set <- set_env()
#' set <- add_many.set_env(set, c("apple", "banana"))
#' objects <- get_objects.set_env(set) # Should return list("apple", "banana")
get_objects.set_env <- function(set) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  object_ids <- ls(envir = set)
  objects <- lapply(object_ids, function(id)
    get(id, envir = set))
  return(objects)
}

# Get all keys####
#' Title Get All Keys in Set
#'
#' @param set The set_env to retrieve keys from.
#'
#' @return A character vector of keys in the set.
#' @export get_keys.set_env
#'
#' @examples
#' set <- set_env()
#' set <- add_many.set_env(set, c("apple", "banana"))
#' keys <- get_keys.set_env(set) # Should return c(digest::digest("apple"), digest::digest("banana"))
get_keys.set_env <- function(set) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  # Retrieve and return all keys (unique identifiers)
  keys <- ls(envir = set)
  # Returns a character vector of keys
  return(keys)
}

# As a set_env####
#' Title Convert Object to set_env
#'
#' @param object An object to convert to a set_env.
#'
#' @return A set_env object containing the elements of the input object.
#' @export as.set_env
#'
#' @examples
#' obj <- c("apple", "banana")
#' set <- as.set_env(obj) # Should return a set_env with "apple" and "banana"
as.set_env <- function(object) {
  set <- set_env()  # Initialize a new set_env object
  if (is.list(object) || is.vector(object)) {
    # If the object is a list or vector, iterate and add each element
    for (elem in object) {
      set <- add.set_env(set, elem)
    }
  } else {
    # For non-iterable objects, add the object directly
    set <- add.set_env(set, object)
  }
  return(set)
}

# Add many items####
#' Title Add Multiple Objects to Set
#'
#' @param set The set_env to which to add the objects.
#' @param items A vector or list of objects to add to the set.
#'
#' @return The set_env with the objects added.
#' @export add_many.set_env
#'
#' @examples
#' set <- set_env()
#' items <- c("apple", "banana", "cherry")
#' set <- add_many.set_env(set, items) # Should add all items to the set
add_many.set_env <- function(set, items) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  # if (!is.vector(items) && !is.list(items)) {
  #   stop("Items must be a vector or a list")
  # }
  for (item in items) {
    set <- add.set_env(set, item)
  }
  return(set)
}


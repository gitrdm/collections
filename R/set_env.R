debug <- FALSE

# Set Constructor ####
set_env <- function() {
  env <- new.env(parent = emptyenv(), hash = TRUE)
  class(env) <- "set_env"
  env
}

# Add Method ####
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
}
#Union####
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
add_many.set_env <- function(set, items) {
  if (!inherits(set, "set_env")) {
    stop("The provided object is not a set_env")
  }
  if (!is.vector(items) && !is.list(items)) {
    stop("Items must be a vector or a list")
  }
  for (item in items) {
    set <- add.set_env(set, item)  # Assuming add.set_env() adds a single item
  }
  return(set)
}


if (debug) {
  # Example Usage
  # Assuming the existence of set_env() constructor and add.set_env() method

  # Create a new set
  mySet <- set_env()

  # Add elements to the set (assuming add.set_env() is implemented)
  mySet <- add.set_env(mySet, "apple")
  mySet <- add.set_env(mySet, "banana")

  # Example Usage
  # Assuming the existence of set_env(), add.set_env(), and other set methods

  # Create a new set
  mySet <- set_env()

  # List of items to add
  items_to_add <- c("apple", "banana", "cherry")

  # Add many items to the set
  mySet <- add_many.set_env(mySet, items_to_add)

  # Assuming get_elements.set_env() retrieves all elements of the set
  elements <- get_objects.set_env(mySet)
  print(elements)  # Should print "apple", "banana", "cherry"

  # Example Usage
  setA <- set_env()
  setA <- add.set_env(setA, "apple")
  setA <- add.set_env(setA, "banana")

  setB <- set_env()
  setB <- add.set_env(setB, "banana")
  setB <- add.set_env(setB, "cherry")


  mySet <- set_env()
  myObject1 <- list(a = 1, b = 2)
  myObject2 <- "Hello, world!"
  mySet <- add.set_env(mySet, myObject1)  # Adding an object
  mySet <- add.set_env(mySet, myObject2)  # Adding another object

  # Retrieve all keys
  keys <- get_keys.set_env(mySet)
  print(keys)

  # Example Usage
  # Example Usage
  mySet <- set_env()
  myObject <- list(a = 1, b = 2)
  mySet <- add.set_env(mySet, myObject)  # Adding an object
  # Check if the object is in the set
  if (contains.set_env(mySet, myObject)) {
    print("The object is in the set.")
  } else {
    print("The object is not in the set.")
  }
  # Example Usage
  mySet <- set_env()
  mySet <- add.set_env(mySet, "apple")

  # Test for membership
  isAppleInSet <- contains.set_env(mySet, "apple")
  isBananaInSet <- contains.set_env(mySet, "banana")

  print(isAppleInSet)  # Should print TRUE
  print(isBananaInSet)  # Should print FALSE

  myList <- list("a", "b", list(1, 2, 3))
  mySetEnv <- as.set_env(myList)
  print(ls(envir = mySetEnv))
  print(get_objects.set_env(mySetEnv))

  # Assuming the set_env class and methods (add.set_env, contains.set_env) are already defined

  # Create a new set_env object
  mySet <- set_env()

  # Benchmark adding elements
  benchmark_add <- microbenchmark::microbenchmark(
    add_10 = for (i in 1:10)
      add.set_env(mySet, runif(1)),
    # Add 10 random numbers
    add_100 = for (i in 1:100)
      add.set_env(mySet, runif(1)),
    # Add 100 random numbers
    times = 100  # Number of times to run each expression for averaging
  )

  # Benchmark checking for membership
  # First, add some elements to the set
  for (i in 1:100)
    add.set_env(mySet, runif(1))
  # Then benchmark membership checking
  benchmark_contains <- microbenchmark::microbenchmark(
    contains_true = contains.set_env(mySet, mySet[[ls(mySet)[1]]]),
    # Check for an element that exists
    contains_false = contains.set_env(mySet, -1),
    # Check for an element that doesn't exist
    times = 100
  )

  # Print the benchmarking results
  print(benchmark_add)
  print(benchmark_contains)

  #June 2024
  #Unit: milliseconds
  #expr      min       lq     mean   median       uq       max neval
  #add_10 1.087994 1.142863 1.353876 1.208798 1.385444  3.106514   100
  #add_100 2.887318 3.015191 3.862823 3.206443 3.937075 12.434191   100
  #Unit: microseconds
  #expr      min         lq        mean     median         uq       max neval
  #contains_true 20921.35 21362.3860 22154.57051 21815.8510 22561.0395 26644.372   100
  #contains_false    16.06    20.7095    43.59144    48.9125    63.4655   163.019   100
}

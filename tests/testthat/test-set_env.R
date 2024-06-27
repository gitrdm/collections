# If you want to some of the tests that push the functionality of the add,
# add_many, and contains functions, you set the environment variable RUN_SET_TESTS to true.
# To run the benchmarks, set the environment variable RUN_BENCHMARKS to true.

withr::with_envvar(c(RUN_SET_TESTS = "true", RUN_BENCHMARKS = "true"), {
  test_that("set_env creates a new set environment", {
    set <- set_env()
    expect_true(inherits(set, "set_env"))
  })
  # Get basic system information
  sys_info <- Sys.info()

  # Determine the operating system
  os_type <- sys_info["sysname"]

  # Initialize an empty string to hold CPU information
  cpu_info <- ""

  # Use conditional logic to execute OS-specific commands
  if (os_type == "Windows") {
    # Windows-specific command
    cpu_info <- system(
      "powershell -command \"Get-WmiObject -Class Win32_Processor | Select-Object -Property Name,NumberOfCores,NumberOfLogicalProcessors,MaxClockSpeed | Format-List\"",
      intern = TRUE
    )
  } else if (os_type == "Linux") {
    # Linux-specific command
    cpu_info <- system("lscpu", intern = TRUE)
  } else if (os_type == "Darwin") {
    # macOS-specific command (Darwin is the underlying OS for macOS)
    cpu_info <- system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
  } else {
    cpu_info <- "Unsupported operating system."
  }

  # Find the line containing "Model name"
  model_line <- grep("Model name", cpu_info, value = TRUE)

  # Extract the model name from the line
  model_name <- sub("Model name:\\s*", "", model_line)

  test_that("add.set_env adds an object to the set", {
    set <- set_env()
    obj <- "apple"
    expect_equal(length(ls(set)), 0)
    set <- add.set_env(set, obj)
    expect_true(contains.set_env(set, obj))
  })

  test_that("remove.set_env removes an object from the set", {
    set <- set_env()
    obj <- "apple"
    set <- add.set_env(set, obj)
    set <- remove.set_env(set, obj)
    expect_false(contains.set_env(set, obj))
  })

  test_that("remove_all.set_env removes all objects from the set", {
    set <- set_env()
    set <- add_many.set_env(set, c("apple", "banana"))
    set <- remove_all.set_env(set)
    expect_equal(length(get_objects.set_env(set)), 0)
  })

  test_that("union.set_env combines two sets without duplicates", {
    set1 <- set_env()
    set1 <- add.set_env(set1, "apple")
    set2 <- set_env()
    set2 <- add.set_env(set2, "banana")
    unionSet <- union.set_env(set1, set2)
    expect_true(all(c("apple", "banana") %in% get_objects.set_env(unionSet)))
    expect_equal(length(get_objects.set_env(unionSet)), 2)
  })

  test_that("intersection.set_env identifies common objects", {
    set1 <- set_env()
    set1 <- add.set_env(set1, "apple")
    set1 <- add.set_env(set1, "banana")
    set2 <- set_env()
    set2 <- add.set_env(set2, "banana")
    intersectionSet <- intersection.set_env(set1, set2)
    expect_true("banana" %in% get_objects.set_env(intersectionSet))
    expect_equal(length(get_objects.set_env(intersectionSet)), 1)
  })

  test_that("difference.set_env identifies unique objects in first set", {
    set1 <- set_env()
    set1 <- add.set_env(set1, "apple")
    set1 <- add.set_env(set1, "banana")
    set2 <- set_env()
    set2 <- add.set_env(set2, "banana")
    differenceSet <- difference.set_env(set1, set2)
    expect_true("apple" %in% get_objects.set_env(differenceSet))
    expect_equal(length(get_objects.set_env(differenceSet)), 1)
  })

  test_that("contains.set_env checks for object presence correctly", {
    set <- set_env()
    obj <- "apple"
    set <- add.set_env(set, obj)
    expect_true(contains.set_env(set, obj))
    expect_false(contains.set_env(set, "banana"))
  })

  test_that("get_objects.set_env retrieves all objects", {
    set <- set_env()
    set <- add_many.set_env(set, c("apple", "banana"))
    objects <- get_objects.set_env(set)
    expect_identical(objects, list("apple", "banana"))
  })

  test_that("get_keys.set_env retrieves all keys", {
    set <- set_env()
    set <- add_many.set_env(set, c("apple", "banana"))
    keys <- get_keys.set_env(set)
    expect_equal(length(keys), 2)
    expect_true(all(c(
      digest::digest("apple"), digest::digest("banana")
    ) %in% keys))
  })

  test_that("as.set_env converts objects to set_env correctly", {
    obj <- c("apple", "banana")
    set <- as.set_env(obj)
    expect_true(all(obj %in% get_objects.set_env(set)))
  })

  test_that("add_many.set_env adds multiple objects", {
    set <- set_env()
    items <- c("apple", "banana", "cherry")
    set <- add_many.set_env(set, items)
    expect_true(all(items %in% get_objects.set_env(set)))
  })

  # Test for adding elements to the set
  test_that("add.set_env adds elements correctly", {
    skip_if(
      !Sys.getenv("RUN_SET_TESTS") == "true",
      "Skipping set tests based on environment variable"
    )

    mySet <- set_env()

    # Add a single element
    add.set_env(mySet, 0.5)
    expect_equal(length(ls(mySet)), 1)

    # Add multiple unique elements
    for (i in 1:10)
      add.set_env(mySet, runif(1))
    expect_equal(length(ls(mySet)), 11)

    # Attempt to add duplicate elements
    add.set_env(mySet, 0.5)
    expect_equal(length(ls(mySet)), 11, info = "Adding a duplicate element should not increase the set size")
  })

  # Test for checking membership ################################################
  test_that("contains.set_env checks membership correctly", {
    skip_if(
      !Sys.getenv("RUN_SET_TESTS") == "true",
      "Skipping set tests based on environment variable"
    )

    mySet <- set_env()

    # Add some elements
    for (i in 1:100)
      add.set_env(mySet, i / 100)

    # Check for an existing element
    expect_true(contains.set_env(mySet, 0.01))

    # Check for a non-existing element
    expect_false(contains.set_env(mySet, -1))
  })

  # Benchmark for set operations ################################################
  test_that("benchmark of set operations", {
    skip_if(
      !Sys.getenv("RUN_BENCHMARKS") == "true",
      "Skipping benchmarks based on environment variable"
    )
    timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

    dir_path <- "benchmarks"
    file_name <- "benchmark-results.txt" # Single file for appending results

    # Check if the directory exists, create it if it doesn't
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    }

    # Benchmark for adding elements
    mySet <- set_env() # Assuming set_env() is defined elsewhere
    bm_add <- microbenchmark::microbenchmark(
      add_single = add.set_env(mySet, 0.5),
      # Assuming add.set_env() is defined elsewhere
      add_multiple = {
        for (i in 1:10)
          add.set_env(mySet, runif(1))
      },
      add_duplicate = add.set_env(mySet, 0.5),
      times = 100
    )

    # Capture and append the benchmark result for adding elements
    #bm_add_summary <- capture.output(summary(bm_add))
    #bm_add_summary_with_timestamp <- c(paste("Timestamp:", timestamp), bm_add_summary) # Add timestamp to the summary
    #cat(bm_add_summary_with_timestamp, file = file.path(dir_path, file_name), append = TRUE, sep = "\n")

    # Benchmark for checking membership
    # Preparing the set with elements
    for (i in 1:100)
      add.set_env(mySet, i / 100)
    bm_contains <- microbenchmark::microbenchmark(
      contains_true = contains.set_env(mySet, 0.01),
      # Assuming contains.set_env() is defined elsewhere
      contains_false = contains.set_env(mySet, -1),
      times = 100
    )

    # Capture and append the benchmark result for checking membership
    #bm_contains_summary <- capture.output(summary(bm_contains))
    #bm_contains_summary_with_timestamp <- c(paste("Timestamp:", timestamp), bm_contains_summary) # Add timestamp to the summary
    #cat(bm_contains_summary_with_timestamp, file = file.path(dir_path, file_name), append = TRUE, sep = "\n")
    bm <- rbind(as.data.frame(bm_add), as.data.frame(bm_contains))
    timestamp_vector <- rep(timestamp, nrow(bm))
    model_name_vector <- rep(model_name, nrow(bm))

    bm$timestamp <- timestamp_vector
    bm$model_name <- model_name_vector

    write.table(
      bm,
      file = file.path(dir_path, file_name),
      append = TRUE,
      row.names = FALSE,
      col.names = !file.exists(file.path(dir_path, file_name)),
      sep = ","
    )

    # Check if the file was written and contains the expected content
    file_exists <- file.exists(file.path(dir_path, file_name))
    file_content <- readLines(file.path(dir_path, file_name))
    content_has_timestamp <- any(grepl(timestamp, file_content))

    # Assert that the file exists and contains a timestamp
    expect_true(file_exists, info = "Benchmark file was not successfully written.")
    expect_true(content_has_timestamp, info = "Benchmark file does not contain a timestamp.")
  })

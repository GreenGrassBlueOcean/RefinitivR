# Script to run RefinitivR streaming examples
#
# This script runs the example files to test streaming functionality
# after recent refactoring changes.

library(Refinitiv)

cat("\n")
cat("=", paste(rep("=", 70), collapse = ""), "=\n")
cat("  REFINITIVR - RUNNING EXAMPLES\n")
cat("=", paste(rep("=", 70), collapse = ""), "=\n")
cat("\n")

# Get example file paths
example_dir <- system.file("examples", package = "Refinitiv")
if (example_dir == "") {
  stop("Examples directory not found. Make sure package is installed.")
}

example_files <- list.files(example_dir, pattern = "\\.R$", full.names = TRUE)

if (length(example_files) == 0) {
  stop("No example files found in ", example_dir)
}

cat(sprintf("Found %d example file(s):\n", length(example_files)))
for (file in example_files) {
  cat(sprintf("  - %s\n", basename(file)))
}
cat("\n")

# Ask user which example to run
cat("Which example would you like to run?\n")
cat("  1. streaming_quick_example.R (Quick test - ~10 seconds)\n")
cat("  2. streaming_complete_example.R (Full feature demo - ~30 seconds)\n")
cat("  3. streaming_test_debug.R (Debug logging test - ~15 seconds)\n")
cat("  4. Run all examples sequentially\n")
cat("  0. Exit\n\n")

choice <- readline("Enter choice (0-4): ")

if (choice == "0") {
  cat("Exiting.\n")
  quit(save = "no")
}

# Map choice to file
file_map <- list(
  "1" = "streaming_quick_example.R",
  "2" = "streaming_complete_example.R",
  "3" = "streaming_test_debug.R"
)

if (choice == "4") {
  # Run all examples
  for (i in 1:3) {
    file_name <- file_map[[as.character(i)]]
    file_path <- file.path(example_dir, file_name)

    if (file.exists(file_path)) {
      cat("\n")
      cat("=", paste(rep("=", 70), collapse = ""), "=\n")
      cat(sprintf("  RUNNING: %s\n", file_name))
      cat("=", paste(rep("=", 70), collapse = ""), "=\n")
      cat("\n")

      tryCatch({
        source(file_path, local = TRUE)
        cat(sprintf("\n✓ %s completed successfully\n", file_name))
      }, error = function(e) {
        cat(sprintf("\n✗ Error running %s: %s\n", file_name, e$message))
      })

      # Pause between examples
      if (i < 3) {
        cat("\nPress Enter to continue to next example...")
        readline()
      }
    }
  }
} else if (choice %in% names(file_map)) {
  # Run single example
  file_name <- file_map[[choice]]
  file_path <- file.path(example_dir, file_name)

  if (!file.exists(file_path)) {
    stop(sprintf("Example file not found: %s", file_path))
  }

  cat("\n")
  cat("=", paste(rep("=", 70), collapse = ""), "=\n")
  cat(sprintf("  RUNNING: %s\n", file_name))
  cat("=", paste(rep("=", 70), collapse = ""), "=\n")
  cat("\n")

  source(file_path, local = TRUE)

  cat("\n")
  cat("=", paste(rep("=", 70), collapse = ""), "=\n")
  cat("  EXAMPLE COMPLETE\n")
  cat("=", paste(rep("=", 70), collapse = ""), "=\n")
  cat("\n")
} else {
  cat("Invalid choice. Exiting.\n")
  quit(save = "no")
}


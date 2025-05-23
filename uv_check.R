##############################################################################
####                     2024-11 NW Data Commons                          ####
##############################################################################

# 0-SETUP ----------------------------------------------------------------------

  # INITIAL SETUP
  rm(list=ls()) #Remove lists
  gc()
  options(java.parameters = "- Xmx8g") #helps R not to fail when importing large xlsx files

  # ESTABLISH BASE DIRECTORIES
  wd <- "/home/wnf/code/nw-data-commons"
  setwd(wd) # Set working directory

  source.tables.dir <- file.path(wd, "1-source-data_nc-files") # Use file.path for platform independence

  # Check if the source directory exists
  if (!dir.exists(source.tables.dir)){
    stop(paste("Error: source.tables.dir does not exist at:", source.tables.dir))
  } else {
    print(paste("Source data directory found at:", source.tables.dir))
  }

  # LOAD LIBRARIES/PACKAGES
  library(wnf.utils)
  LoadCommonPackages() # Assuming this loads a set of commonly used packages
  library(googledrive)
  try(drive_auth(email = "william@fluxrme.com"), silent = TRUE) # Authenticate Google Drive, wrap in try in case it fails in some environments
  library(purrr)
  library(ncdf4)
  library(janitor)
  library(lubridate)
  library(openxlsx)
  library(dplyr)  # Load dplyr for data manipulation
  library(tidyr)  # Load tidyr for reshaping data
  library(sf)     # Load sf for geospatial operations (you'll need this later)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(viridis)
  library(raster)
  library(shiny)

# 1-CONFIGURATION ----------------------------------------------------------------------

  # Functions to inspect .nc files to help with updating configs
  inspect_nc_file <- function(file_path) {
    # Load ncdf4 in case it isn't already
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
      stop("Package 'ncdf4' is required but not installed.")
    }
    
    nc <- ncdf4::nc_open(file_path)
    on.exit(ncdf4::nc_close(nc))  # Ensure we close the file on function exit
    
    cat("============================================================\n")
    cat("INSPECTING NETCDF FILE:\n")
    cat("  ", basename(file_path), "\n")
    cat("============================================================\n\n")
    
    # 1. DIMENSIONS
    cat("DIMENSIONS:\n")
    dim_names <- names(nc$dim)
    if (length(dim_names) == 0) {
      cat("  [No dimensions found]\n\n")
    } else {
      for (dn in dim_names) {
        dim_info <- nc$dim[[dn]]
        cat("  -", dn, ": length =", dim_info$len, "\n")
      }
      cat("\n")
    }
    
    # 2. VARIABLES
    cat("VARIABLES:\n")
    var_names <- names(nc$var)
    if (length(var_names) == 0) {
      cat("  [No variables found]\n")
    } else {
      for (vn in var_names) {
        var_info <- nc$var[[vn]]
        # Each variable's 'dim' is a list of dimension objects (each with name, length, etc.)
        dim_str <- paste(sapply(var_info$dim, function(d) d$name), collapse = ", ")
        cat("  -", vn, "(dimensions:", dim_str, ")\n")
      }
    }
    
    cat("\n------------------------------------------------------------\n\n")
  }

  inspect_nc_directory <- function(dir_path) {
    files <- list.files(dir_path, pattern = "\\.nc$", full.names = TRUE)
    if (length(files) == 0) {
      cat("No .nc files found in:", dir_path, "\n")
      return(invisible(NULL))
    }
    
    for (f in files) {
      inspect_nc_file(f)
    }
  }

  # Define the structure of your model outputs. Each entry now points to a directory.
  configs.ls <- list(
    #"temp_precip" = list(
    #  directory = file.path(source.tables.dir, "1-2-temp-precip"),
    #  file_pattern = ".*\\.nc$", # Matches any file ending with .nc
    #  variables = c("TS", "TSMN", "TSMX", "PRECC", "PRECL"),
    #  time_var = "time",
    #  lat_var = "lat",
    #  lon_var = "lon",
    #  time_bnds_var = "time_bnds"  # <-- Add this if your .nc files have time boundaries
    #),
    "uv" = list(
      directory = file.path(source.tables.dir, "3-uv"),
      file_pattern = ".*\\.nc$",
      variables = c("TUV_UVA", "TUV_uva", "TUV_UVC", "TUV_UVINDEX", "TUV_UVINDEXMX"),
      time_var = "time",
      lat_var = "lat",
      lon_var = "lon"
      # no time_bnds_var here because UV files might not have it
    )
  )

# 2-IMPORT ----------------------------------------------------------------------

  # (1) Helper function to flatten a single variable, respecting netCDF dimension order:
  flatten_variable <- function(nc_file, var_name, config) {
    # var_name must exist
    if (! (var_name %in% names(nc_file$var))) {
      stop("Variable '", var_name, "' not found in netCDF file.")
    }
    
    # 1. Grab dimension metadata for the variable
    var_info  <- nc_file$var[[var_name]]
    dims_info <- var_info$dim    # list of dimension objects, in netCDF’s declared order
    if (length(dims_info) != 3) {
      stop("Variable '", var_name, "' does not have exactly 3 dims (lon, lat, time). Found: ", length(dims_info))
    }
    
    # 2. We expect dims in the order: (lon, lat, time).
    #    If your netCDF has them in a different order, either reorder or fail.
    #    Here we strictly check the dimension names match config$lon_var, etc., in sequence.
    dim_names <- sapply(dims_info, `[[`, "name")
    expected_names <- c(config$lon_var, config$lat_var, config$time_var)
    
    if (!all(dim_names == expected_names)) {
      stop("Variable '", var_name, "' dims are ", paste(dim_names, collapse=", "),
          ", but expected (", paste(expected_names, collapse=", "), ").")
    }
    
    # 3. For each dimension, get the .vals
    #    Stop if they’re missing or empty.
    lon_dim <- nc_file$dim[[config$lon_var]]
    lat_dim <- nc_file$dim[[config$lat_var]]
    time_dim <- nc_file$dim[[config$time_var]]
    
    if (is.null(lon_dim$vals) || is.null(lat_dim$vals) || is.null(time_dim$vals)) {
      stop("One or more dimension variables (lon, lat, time) have no '$vals' in netCDF file.")
    }
    
    lon_vals <- lon_dim$vals
    lat_vals <- lat_dim$vals
    time_vals <- time_dim$vals
    
    # 4. Read the actual variable data, expecting shape = c(length(lon), length(lat), length(time))
    var_data <- ncdf4::ncvar_get(nc_file, var_name, collapse_degen = FALSE)
    dim_var <- dim(var_data)
    expected_shape <- c(length(lon_vals), length(lat_vals), length(time_vals))
    
    if (!identical(dim_var, expected_shape)) {
      stop("Variable '", var_name, "' has shape ", paste(dim_var, collapse="x"),
          ", expected ", paste(expected_shape, collapse="x"), ".")
    }
    
    # 5. Build expand.grid(...) in the same dimension order (lon, lat, time)
    df_coords <- expand.grid(
      lon = lon_vals,
      lat = lat_vals,
      time = time_vals,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    
    # 6. Flatten var_data using as.vector(...) in column-major order
    df_coords[[var_name]] <- as.vector(var_data)
    
    return(dplyr::as_tibble(df_coords))
  }


  # (2) Generalized function to import & flatten a single .nc file
  import_single_nc <- function(file_path, config) {
    tryCatch({
      message("Processing file: ", file_path)
      nc_file <- ncdf4::nc_open(file_path)
      on.exit(ncdf4::nc_close(nc_file))
      
      # If you want to forcibly check that the file *has* dim named config$lon_var, etc.
      # you can do that here or rely on flatten_variable to throw an error.
      
      # Flatten each target variable & merge
      nc_file.df <- NULL
      for (var_name in config$variables) {
        # We call the new flatten_variable
        var_df <- flatten_variable(nc_file, var_name, config)
        
        if (is.null(nc_file.df)) {
          nc_file.df <- var_df
        } else {
          # Merge on the (lon, lat, time) columns
          dim_cols <- c("lon", "lat", "time")
          nc_file.df <- dplyr::left_join(nc_file.df, var_df, by = dim_cols)
        }
      }
      
      if (is.null(nc_file.df)) {
        warning("No data imported for file: ", basename(file_path))
        return(NULL)
      }
      
      # Store the filename as a variable
      nc_file.df %<>% mutate(file_name = basename(file_path))
      
      return(nc_file.df)
      
    }, error = function(e) {
      warning("Error importing file '", file_path, "': ", e$message)
      return(NULL)
    })
  }


  # (3) Function to import data for a single model output
  import_model_data <- function(model_name, config) {
    print(paste("Importing data for model output:", model_name))
    directory <- config$directory
    file_pattern <- config$file_pattern

    if (!dir.exists(directory)) {
      warning(paste("Directory not found for model output", model_name, "at:", directory))
      return(NULL)
    }

    all_files <- list.files(directory, pattern = file_pattern, full.names = TRUE, recursive = TRUE)
    num_files <- length(all_files)

    if (num_files == 0) {
      warning(paste("No .nc files found for model output", model_name, "in:", directory))
      return(NULL)
    }

    # Process all found .nc files with progress indicator
    model_data_list <- purrr::map2(all_files, seq_len(num_files), function(file_path, index) {
      result <- import_single_nc(file_path, config)
      percentage_complete <- round((index / num_files) * 100, 1)
      message(paste0("  ", percentage_complete, "% (", index, "/", num_files, ") files processed for ", model_name))
      return(result)
    }) %>%
      purrr::compact()

    if (length(model_data_list) > 0) {
      # Combine all files for a single model into one tibble
      combined_data <- dplyr::bind_rows(model_data_list) %>%
        dplyr::mutate(model_output = model_name)
      return(combined_data)
    } else {
      warning(paste("No data imported for model output:", model_name))
      return(NULL)
    }
  }

# 3-FLATTEN NETCDF FILES (Intermediate Product) ----------------------------------------------------------------------

  # Apply the import function to all configured model outputs
  gridded_tables.ls <- 
    purrr::map(names(configs.ls), function(model_name) {
      import_model_data(model_name, configs.ls[[model_name]])
    }) %>%
    rlang::set_names(names(configs.ls)) %>%
    purrr::compact()

# CHECKING EQUALITY OF UVB & UVC IN .NC FILES

  compare_uv_variables <- function(file_paths, config) {
    results <- list()

    var_pairs <- list(
      c("TUV_UVA", "TUV_UVB"),
      c("TUV_UVA", "TUV_UVC"),
      c("TUV_UVB", "TUV_UVC")
    )

    for (file_path in file_paths) {
      nc <- ncdf4::nc_open(file_path)
      on.exit(ncdf4::nc_close(nc), add = TRUE)

      # Load the raw variable arrays
      uva_vals <- as.vector(ncdf4::ncvar_get(nc, "TUV_UVA"))
      uvb_vals <- as.vector(ncdf4::ncvar_get(nc, "TUV_UVB"))
      uvc_vals <- as.vector(ncdf4::ncvar_get(nc, "TUV_UVC"))

      for (pair in var_pairs) {
        v1 <- pair[1]
        v2 <- pair[2]

        # Pull the arrays
        vals1 <- get(tolower(gsub("TUV_", "", v1)) %>% paste0("_vals"))
        vals2 <- get(tolower(gsub("TUV_", "", v2)) %>% paste0("_vals"))

        # Remove cases where both are 0 (true no-UV conditions)
        nonzero_mask <- !(vals1 == 0 & vals2 == 0)
        vals1 <- vals1[nonzero_mask]
        vals2 <- vals2[nonzero_mask]

        # Do the comparison
        is_equal <- vals1 == vals2
        abs_diff <- abs(vals1 - vals2)

        summary_row <- tibble(
          file_name = basename(file_path),
          var_pair = paste(v1, v2, sep = " vs "),
          total_values = length(vals1),
          num_identical = sum(is_equal),
          prop_identical = mean(is_equal),
          mean_abs_diff = mean(abs_diff),
          min_abs_diff = min(abs_diff),
          max_abs_diff = max(abs_diff)
        )

        results[[length(results) + 1]] <- summary_row
      }
    }

    results_df <- bind_rows(results)
    return(results_df)
  }

  # -----------------------------
  # Run it on all UV .nc files
  # -----------------------------
  uv_config <- configs.ls[["uv"]]
  uv_files <- list.files(uv_config$directory, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)

  uv_comparison_summary <- compare_uv_variables(uv_files, uv_config)

  # View the summary
  print(uv_comparison_summary)
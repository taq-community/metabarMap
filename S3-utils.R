# =============================================================================
# Utility Functions for OpenStack S3 Data Access
# =============================================================================
# This module contains reusable functions for connecting to and reading data
# from OpenStack object storage using the S3-compatible API

#' Configure OpenStack S3 Connection
#'
#' @param endpoint_url URL of the OpenStack S3 endpoint
#' @param access_key OpenStack access key (optional, reads from env if NULL)
#' @param secret_key OpenStack secret key (optional, reads from env if NULL)
#' @return The configured endpoint URL
#' @export
configure_openstack_s3 <- function(endpoint_url = NULL, 
                                   access_key = NULL, 
                                   secret_key = NULL) {
  
  # Set endpoint
  if (is.null(endpoint_url)) {
    endpoint_url <- Sys.getenv("AWS_S3_ENDPOINT", "")
  }
  
  if (endpoint_url == "") {
    stop("OpenStack endpoint not specified. Set AWS_S3_ENDPOINT environment variable or provide endpoint_url parameter.")
  }
  
  Sys.setenv("AWS_S3_ENDPOINT" = endpoint_url)
  
  # Set credentials if provided
  if (!is.null(access_key)) {
    Sys.setenv("AWS_ACCESS_KEY_ID" = access_key)
  }
  
  if (!is.null(secret_key)) {
    Sys.setenv("AWS_SECRET_ACCESS_KEY" = secret_key)
  }
  
  # Verify credentials are set
  if (Sys.getenv("AWS_ACCESS_KEY_ID", "") == "" || 
      Sys.getenv("AWS_SECRET_ACCESS_KEY", "") == "") {
    warning("OpenStack credentials not fully configured. Set AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY.")
  }
  
  return(endpoint_url)
}

#' Get OpenStack S3 Endpoint
#'
#' @return The configured endpoint URL
#' @export
get_s3_endpoint <- function() {
  endpoint <- Sys.getenv("AWS_S3_ENDPOINT", "")
  if (endpoint == "") {
    stop("OpenStack S3 endpoint not configured. Please set AWS_S3_ENDPOINT.")
  }
  return(endpoint)
}

#' List Files in OpenStack S3 Bucket
#'
#' @param bucket Bucket name
#' @param prefix File prefix to filter by
#' @param pattern Regex pattern to filter file names
#' @param endpoint OpenStack endpoint URL (optional)
#' @return Character vector of file keys
#' @export
list_s3_files <- function(bucket, 
                          prefix = "", 
                          pattern = NULL, 
                          endpoint = NULL) {
  
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop("Package 'aws.s3' is required. Install with: install.packages('aws.s3')")
  }
  
  tryCatch({
    if (is.null(endpoint)) {
      endpoint <- get_s3_endpoint()
    }
    
    # Get bucket contents
    objects <- aws.s3::get_bucket_df(
      bucket = bucket, 
      prefix = "",
      base_url = endpoint,
      region = "",
      use_https = TRUE
    )
    
    # Filter by pattern if provided
    if (!is.null(pattern) && nrow(objects) > 0) {
      objects <- objects[grepl(pattern, objects$Key), ]
    }
    s5cmd
    if (nrow(objects) == 0) {
      message("No files found matching criteria")
      return(character(0))
    }
    
    return(objects$Key)
    
  }, error = function(e) {
    message("Error listing files: ", e$message)
    return(character(0))
  })
}

#' Load Data from OpenStack S3
#'
#' @param bucket Bucket name
#' @param key File key/path
#' @param file_type File type: "csv", "rds", "parquet", "xlsx", "tsv"
#' @param endpoint OpenStack endpoint URL (optional)
#' @param ... Additional parameters passed to read functions
#' @return Data frame or NULL if error
#' @export
load_s3_data <- function(bucket, 
                         key, 
                         file_type = "csv", 
                         endpoint = NULL,
                         ...) {
  
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop("Package 'aws.s3' is required. Install with: install.packages('aws.s3')")
  }
  
  tryCatch({
    if (is.null(endpoint)) {
      endpoint <- get_s3_endpoint()
    }
    
    # Get object from OpenStack S3
    obj <- aws.s3::get_object(
      object = key, 
      bucket = bucket,
      base_url = endpoint,
      region = "",
      use_https = TRUE
    )
    
    # Parse based on file type
    data <- switch(
      tolower(file_type),
      
      "csv" = {
        read.csv(text = rawToChar(obj), stringsAsFactors = FALSE, ...)
      },
      
      "tsv" = {
        read.delim(text = rawToChar(obj), stringsAsFactors = FALSE, ...)
      },
      
      "rds" = {
        readRDS(rawConnection(obj))
      },
      
      "parquet" = {
        if (!requireNamespace("arrow", quietly = TRUE)) {
          stop("Package 'arrow' required for parquet files. Install with: install.packages('arrow')")
        }
        temp_file <- tempfile(fileext = ".parquet")
        writeBin(obj, temp_file)
        data <- arrow::read_parquet(temp_file, ...)
        unlink(temp_file)
        data
      },
      
      "xlsx" = {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' required for Excel files. Install with: install.packages('readxl')")
        }
        temp_file <- tempfile(fileext = ".xlsx")
        writeBin(obj, temp_file)
        data <- readxl::read_excel(temp_file, ...)
        unlink(temp_file)
        data
      },
      
      stop("Unsupported file type: ", file_type, 
           ". Supported types: csv, tsv, rds, parquet, xlsx")
    )
    
    message("Successfully loaded data: ", nrow(data), " rows × ", ncol(data), " columns")
    return(data)
    
  }, error = function(e) {
    message("Error loading data: ", e$message)
    return(NULL)
  })
}

#' Save Data to OpenStack S3
#'
#' @param data Data frame to save
#' @param bucket Bucket name
#' @param key File key/path where data will be saved
#' @param file_type File type: "csv", "rds", "parquet"
#' @param endpoint OpenStack endpoint URL (optional)
#' @return TRUE if successful, FALSE otherwise
#' @export
save_s3_data <- function(data, 
                         bucket, 
                         key, 
                         file_type = "csv",
                         endpoint = NULL) {
  
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop("Package 'aws.s3' is required. Install with: install.packages('aws.s3')")
  }
  
  tryCatch({
    if (is.null(endpoint)) {
      endpoint <- get_s3_endpoint()
    }
    
    # Create temporary file
    temp_file <- tempfile(fileext = paste0(".", file_type))
    
    # Write data to temp file based on type
    switch(
      tolower(file_type),
      
      "csv" = {
        write.csv(data, temp_file, row.names = FALSE)
      },
      
      "rds" = {
        saveRDS(data, temp_file)
      },
      
      "parquet" = {
        if (!requireNamespace("arrow", quietly = TRUE)) {
          stop("Package 'arrow' required for parquet files. Install with: install.packages('arrow')")
        }
        arrow::write_parquet(data, temp_file)
      },
      
      stop("Unsupported file type for saving: ", file_type)
    )
    
    # Upload to S3
    result <- aws.s3::put_object(
      file = temp_file,
      object = key,
      bucket = bucket,
      base_url = endpoint,
      region = "",
      use_https = TRUE
    )
    
    # Clean up
    unlink(temp_file)
    
    message("Successfully saved data to: ", key)
    return(TRUE)
    
  }, error = function(e) {
    message("Error saving data: ", e$message)
    return(FALSE)
  })
}

#' Get Bucket Information
#'
#' @param bucket Bucket name
#' @param endpoint OpenStack endpoint URL (optional)
#' @return List with bucket metadata
#' @export
get_bucket_info <- function(bucket, endpoint = NULL) {
  
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop("Package 'aws.s3' is required. Install with: install.packages('aws.s3')")
  }
  
  tryCatch({
    if (is.null(endpoint)) {
      endpoint <- get_s3_endpoint()
    }
    
    objects <- aws.s3::get_bucket_df(
      bucket = bucket,
      base_url = endpoint,
      region = ""
    )
    
    info <- list(
      bucket_name = bucket,
      total_files = nrow(objects),
      total_size_bytes = sum(as.numeric(objects$Size), na.rm = TRUE),
      last_modified = max(objects$LastModified, na.rm = TRUE)
    )
    
    return(info)
    
  }, error = function(e) {
    message("Error getting bucket info: ", e$message)
    return(NULL)
  })
}

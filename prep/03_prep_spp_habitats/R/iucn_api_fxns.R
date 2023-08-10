library(parallel)
library(jsonlite)


api_version <- '2022-2'

api_key = read.csv(file.path(rdsi_raw_data_dir, "iucn/api_key/api_token.txt"))
api_key = colnames(api_key)


get_from_api <- function(url, param, api_key, delay) {
  
  i <- 1; tries <- 5; success <- FALSE
  
  while(i <= tries & success == FALSE) {
    message('try #', i)
    Sys.sleep(delay * i) ### be nice to the API server? later attempts wait longer
    api_info <- fromJSON(sprintf(url, param, api_key))
    if (class(api_info) != 'try-error') {
      success <- TRUE
    } else {
      warning(sprintf('try #%s: class(api_info) = %s\n', i, class(api_info)))
    }
    message('... successful? ', success)
    i <- i + 1
  }
  
  if (class(api_info) == 'try-error') { ### multi tries and still try-error
    api_return <- data.frame(param_id  = param,
                             api_error = 'try-error after multiple attempts')
  } else if (class(api_info$result) != 'data.frame') { ### result isn't data frame for some reason
    api_return <- data.frame(param_id  = param,
                             api_error = paste('non data.frame output: ', class(api_info$result), ' length = ', length(api_info$result)))
  } else if (length(api_info$result) == 0) { ### result is empty
    api_return <- data.frame(param_id  = param,
                             api_error = 'zero length data.frame')
  } else {
    api_return <- api_info %>%
      data.frame(stringsAsFactors = FALSE)
  }
  
  return(api_return)
}

mc_get_from_api <- function(url, param_vec, api_key, cores = NULL, delay = 0.5) {
  
  if(is.null(cores)) 
    numcores <- ifelse(Sys.info()[['nodename']] == 'mazu', 12, 1)
  else 
    numcores <- cores
  
  out_list <- parallel::mclapply(param_vec, 
                                 function(x) get_from_api(url, x, api_key, delay),
                                 mc.cores   = numcores,
                                 mc.cleanup = TRUE) 
  
  if(any(sapply(out_list, class) != 'data.frame')) {
    error_list <- out_list[sapply(out_list, class) != 'data.frame']
    message('List items are not data frame: ', paste(sapply(error_list, class), collapse = '; '))
    message('might be causing the bind_rows() error; returning the raw list instead')
    return(out_list)
  }
  
  out_df <- out_list %>%
    bind_rows()
  out_df <- out_df %>%
    setNames(names(.) %>%
               str_replace('result.', ''))
  return(out_df)
}
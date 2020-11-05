#' export function to get analysis results
#'
#' @param analysis_type analysis type (poincare, runs, spectral)
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @param clicked_file number of clicked file or NULL
#' @param asym_comparisons comparisons for dynamic asymmetry analysis
#' @param flags_coding list with flags_coding
#'
#' @return the results of Poincare plot analysis
#' @export
get_dynamic_numerical_results <- function(analysis_type,
                                  fileAddresses,
                                  separator = "\t",
                                  column_data = c(1,2),
                                  minmax = c(0, 3000),
                                  using_excel = FALSE,
                                  use_ULF = "No",
                                  window_type,
                                  move_type,
                                  window_length,
                                  clicked_file = NULL,
                                  asym_comparisons = NULL,
                                  flags_coding) {
  if (analysis_type == "poincare_dynamic")
    return(get_dynamic_pp_results(fileAddresses,
                                  time_functions_list = glb_time_functions,
                                  separator = separator,
                                  column_data = column_data,
                                  minmax = minmax,
                                  using_excel = using_excel,
                                  window_type = window_type,
                                  move_type = move_type,
                                  window_length = window_length,
                                  clicked_file = clicked_file,
                                  asym_comparisons = asym_comparisons,
                                  flags_coding = flags_coding))
  if (analysis_type == "runs_dynamic")
    return(get_dynamic_runs_results(fileAddresses,
                                    time_functions_list = glb_time_functions,
                                    separator = separator,
                                    column_data = column_data,
                                    minmax = minmax,
                                    using_excel = using_excel,
                                    window_type = window_type,
                                    move_type = move_type,
                                    window_length = window_length,
                                    clicked_file = clicked_file,
                                    asym_comparisons = asym_comparisons,
                                    flags_coding = flags_coding))
  if (analysis_type == "spectral_dynamic")
    return(get_dynamic_spectral_results(fileAddresses,
                                        time_functions_list = glb_time_functions,
                                        separator = separator,
                                        column_data = column_data,
                                        minmax = minmax,
                                        using_excel = using_excel,
                                        use_ULF = use_ULF,
                                        window_type = window_type,
                                        move_type = move_type,
                                        window_length = window_length,
                                        clicked_file = clicked_file,
                                        flags_coding = flags_coding))
  if (analysis_type == "quality_dynamic")
    return(get_dynamic_quality_results(fileAddresses,
                                       time_functions_list = glb_time_functions,
                                       separator = separator,
                                       column_data = column_data,
                                       minmax = minmax,
                                       using_excel = using_excel,
                                       window_type = window_type,
                                       move_type = move_type,
                                       window_length = window_length,
                                       clicked_file = clicked_file,
                                       flags_coding = flags_coding))
}

#' function for getting the results of dynamic Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @param clicked_file number of clicked file or NULL
#' @param flags_coding list with flags_coding
#'
#' @return the results of Poincare plot analysis
get_dynamic_pp_results <- function(fileAddresses,
                                   time_functions_list = glb_time_functions,
                                   separator = "\t",
                                   column_data = c(1, 2),
                                   minmax = c(0, 3000),
                                   using_excel = FALSE,
                                   window_type,
                                   move_type,
                                   window_length,
                                   clicked_file = NULL,
                                   asym_comparisons = NULL,
                                   flags_coding) {
  results <- c()
  if (!is.null(clicked_file)) {
    rr_and_flags <- read_and_filter_one_file(fileAddresses, clicked_file, separator, column_data, minmax, using_excel, flags_coding)
    single_file_result <- get_single_pp_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                         time_functions_list = time_functions_list,
                                                         window_type = window_type,
                                                         move_type = move_type,
                                                         window_length = window_length) %>%
      round(digits = 3)
    return(dplyr::bind_cols(tibble(`win NO` = seq(nrow(single_file_result))), single_file_result))
  } else {
    for (lineNumber in  1:length(fileAddresses[[1]])) {
      rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding)
      #browser()
      temp_results <- get_single_pp_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                     time_functions_list = time_functions_list,
                                                     window_type = window_type,
                                                     move_type = move_type,
                                                     window_length = window_length) %>%
        round_and_summarize_dynamic_asym(round_digits = 3, asym_comparisons = asym_comparisons)
      results <- rbind(results, temp_results)
    }# TUTU
    results <- cbind(fileAddresses$name, results)
    colnames(results)[1] <- "file"
    rownames(results) <- NULL
    return(results)
  }
}

#' function for getting the results of dynamic runs analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @param clicked_file number of clicked file or NULL
#' @param flags_coding list with flags_coding
#'
#' @return the results of Poincare plot analysis
get_dynamic_runs_results <- function(fileAddresses,
                                     time_functions_list = glb_time_functions,
                                     separator = "\t",
                                     column_data = c(1, 2),
                                     minmax = c(0, 3000),
                                     using_excel = FALSE,
                                     window_type,
                                     move_type,
                                     window_length,
                                     clicked_file = NULL,
                                     asym_comparisons = NULL,
                                     flags_coding) {
  results <- c()
  if (!is.null(clicked_file)) {
    rr_and_flags <- read_and_filter_one_file(fileAddresses, clicked_file, separator, column_data, minmax, using_excel, flags_coding)
    single_file_result <- get_single_runs_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                           time_functions_list = time_functions_list,
                                                           window_type = window_type,
                                                           move_type = move_type,
                                                           window_length = window_length)
    # single_file_result[, -1] <- round(single_file_result[, -1], digits = 3)
    return(dplyr::bind_cols(tibble(`win NO` = seq(nrow(single_file_result))), single_file_result))
  } else {
    for (lineNumber in  1:length(fileAddresses[[1]])){
      rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding)
      temp_results <- get_single_runs_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                       time_functions_list = time_functions_list,
                                                       window_type = window_type,
                                                       move_type = move_type,
                                                       window_length = window_length) %>%
        dplyr::select(-c("file")) %>%
        round_and_summarize_dynamic_asym(round_digits = 3, asym_comparisons = asym_comparisons) %>%
        as.data.frame()
      results <- plyr::rbind.fill(results, temp_results) # rbinding columns with potentially different cols
    }
    results[is.na(results)] <- 0
    results <- cbind(fileAddresses$name, results)
    colnames(results)[1] <- "file"
    rownames(results) <- NULL
    return(results %>% sort_out_runs())
  }
}

#' function for getting the results of Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @param clicked_file number of clicked file or NULL
#' @param flags_coding list with flags_coding
#'
#' @return the results of Poincare plot analysis
get_dynamic_spectral_results <- function(fileAddresses,
                                         use_ULF = "No",
                                         time_functions_list = glb_time_functions,
                                         separator = "\t",
                                         column_data = c(1, 2),
                                         minmax = c(0, 3000),
                                         using_excel = FALSE,
                                         window_type,
                                         move_type,
                                         window_length,
                                         clicked_file,
                                         flags_coding) {
  results <- c()
  if (!is.null(clicked_file)) {
    rr_and_flags <- read_and_filter_one_file(fileAddresses, clicked_file, separator, column_data, minmax, using_excel, flags_coding)
    single_file_result <- get_single_spectral_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                         use_ULF = use_ULF,
                                                         time_functions_list = time_functions_list,
                                                         window_type = window_type,
                                                         move_type = move_type,
                                                         window_length = window_length) %>%
      round(digits = 3)
    return(dplyr::bind_cols(tibble(`win NO` = seq(nrow(single_file_result))), single_file_result))
  } else {
    for (lineNumber in  1:length(fileAddresses[[1]])) {
      rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding)
      temp_results <- get_single_spectral_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                           use_ULF = use_ULF,
                                                           time_functions_list = time_functions_list,
                                                           window_type = window_type,
                                                           move_type = move_type,
                                                           window_length = window_length) %>%
        colMeans(na.rm = TRUE)
      results <- rbind(results, temp_results)
    }
    results <- as.data.frame(round(results, 3))
    results <- cbind(fileAddresses$name, results)
    colnames(results)[1] <- "file"
    rownames(results) <- NULL
    return(results)
  }
}

#' function for getting the results of dynamic Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @param clicked_file number of clicked file or NULL
#' @param flags_coding list with flags_coding
#'
#' @return the results of Poincare plot analysis
get_dynamic_quality_results <- function(fileAddresses,
                                        time_functions_list = glb_time_functions,
                                        separator = "\t",
                                        column_data = c(1, 2),
                                        minmax = c(0, 3000),
                                        using_excel = FALSE,
                                        window_type,
                                        move_type,
                                        window_length,
                                        clicked_file,
                                        flags_coding) {
  results <- c()
  if (!is.null(clicked_file)) {
    rr_and_flags <- read_and_filter_one_file(fileAddresses, clicked_file, separator, column_data, minmax, using_excel, flags_coding)
    temp_results <- get_single_quality_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                        time_functions_list = time_functions_list,
                                                        window_type = window_type,
                                                        move_type = move_type,
                                                        window_length = window_length)
  } else {
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding)
    temp_results <- get_single_quality_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                        time_functions_list = time_functions_list,
                                                        window_type = window_type,
                                                        move_type = move_type,
                                                        window_length = window_length) %>%
      colMeans(na.rm = TRUE)
    results <- rbind(results, temp_results)
  }
  results <- as.data.frame(round(results,3))
  results <- cbind(fileAddresses$name, results)
  colnames(results)[1] <- "file"
  rownames(results) <- NULL
  return(results)
  }
}

#' time window functions as a list
#' @export
glb_time_functions <- list(time_jump = hrvhra::time_based_jump,
                           time_slide = hrvhra::time_based_slide,
                           index_jump = hrvhra::index_based_jump,
                           index_slide = hrvhra::index_based_slide)

#' Helper function cutting either the first row of the table, or the last row, or returning the whole table
#' @param resulting_table table with results from a windowing function
#' @param cut_end where the cut occurs -at the beginning or at the end or not at all (FALSE, TRUE)
#' @param return_all wheter all rows should be returned
#' @return table
cut_incomplete_rows <- function(resulting_table, cut_end, return_all) {
  if (return_all) {
    return(resulting_table)
  }
  if (cut_end) {
    resulting_table[1:(nrow(resulting_table) - 1), ]
  } else {
    resulting_table[2:(nrow(resulting_table)), ]
  }
}

#' Function calculating windowed hrvhra results for a single RR time series
#' @param RR rr object
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @return data.frame with results for windows as rows
#' @export
get_single_pp_windowed_results <- function(RR,
                                           time_functions_list = glb_time_functions,
                                           window_type = "time",
                                           move_type = "jump",
                                           window_length = 5,
                                           cut_end = FALSE,
                                           return_all = FALSE) {
  window_slide = paste(move_type, window_type, sep = "_")
  rr_index <- 'if' (move_type == 'time', 2, 1) # index based windows do not have time track
  time_function <- time_functions_list[[window_slide]]
  lapply('if' (window_type == 'jump', # cut end is only applicable to the jump window type
               time_function(RR, window = window_length, cut_end = cut_end),
               time_function(RR, window = window_length)
        ),
         function(window_table) {
           ret_val <- NULL
           if (move_type == 'index' && nrow(window_table) == window_length) {
             ret_val <- hrvhra::hrvhra(window_table[[rr_index]], window_table[[rr_index + 1]])
           }
           if (move_type == 'time') {
             ret_val <- hrvhra::hrvhra(window_table[[rr_index]], window_table[[rr_index + 1]])
           }
           ret_val
         }) %>%
    dplyr::bind_rows() %>% #TODO the list below, after cut_end is fixed in index based
    cut_incomplete_rows(cut_end, return_all = 'if'(move_type == 'index', TRUE, FALSE))
}

#' Function calculating windowed runs results for a single RR time series
#' @param RR rr object
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @return data.frame with results for windows as rows
#' @export
get_single_runs_windowed_results <- function(RR,
                                             time_functions_list = glb_time_functions,
                                             window_type = "jump",
                                             move_type = "time",
                                             window_length = 5,
                                             cut_end = FALSE,
                                             return_all = FALSE) {
  window_slide = paste(move_type, window_type, sep = "_")
  rr_index <- 'if' (move_type == 'time', 2, 1) # index based windows do not have time track
  time_function <- time_functions_list[[window_slide]]
  runs_list <- lapply('if' (window_type == 'jump', # cut end is only applicable to the jump window type
                            time_function(RR, window = window_length, cut_end = cut_end),
                            time_function(RR, window = window_length)),
                      function(window_table) {
                        ret_val <- NULL
                        if (move_type == 'index' && nrow(window_table) == window_length) {
                          ret_val <- hrvhra::countruns(window_table[[rr_index]], window_table[[rr_index + 1]])
                        }
                        if (move_type == 'time') {
                          ret_val <- hrvhra::countruns(window_table[[rr_index]], window_table[[rr_index + 1]])
                        }
                        ret_val
                      }) %>% Filter(function(elem) !is.null(elem), .)

  hrvhra::bind_runs_as_table(runs_list, as.character(seq_along(runs_list))) %>%
    cut_incomplete_rows(cut_end, return_all = 'if'(move_type == 'index', TRUE, FALSE))
}

#' Function calculating windowed spectral results for a single RR time series
#' @param RR rr object
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @return data.frame with results for windows as rows
#' @export
get_single_spectral_windowed_results <- function(RR,
                                                 time_functions_list = glb_time_functions,
                                                 window_type = "jump",
                                                 move_type = "time",
                                                 use_ULF = "No",
                                                 window_length = 5,
                                                 cut_end = FALSE,
                                                 return_all = FALSE) {
  window_slide = paste(move_type, window_type, sep = "_")
  rr_index <- 'if' (move_type == 'time', 2, 1) # index based windows do not have time track
  time_function <- time_functions_list[[window_slide]]
  bands <- if (use_ULF == "Yes") {
    hrvhra::frequency_bands_24
  } else {
    hrvhra::frequency_bands
  }
  lapply('if' (window_type == 'jump', # cut end is only applicable to the jump window type
               time_function(RR, window = window_length, cut_end = cut_end),
               time_function(RR, window = window_length)),
         function(window_table) {
           ret_val <- NULL
           if (move_type == 'index' && nrow(window_table) == window_length) {
              ret_val <- hrvhra::calculate_RR_spectrum(data.frame(RR = window_table[[rr_index]], annotations = window_table[[rr_index + 1]]), bands)
           }
           if (move_type == 'time') {
             ret_val <- hrvhra::calculate_RR_spectrum(data.frame(RR = window_table[[rr_index]], annotations = window_table[[rr_index + 1]]), bands)
           }
           ret_val
         }) %>%
    dplyr::bind_rows() %>%
    cut_incomplete_rows(cut_end, return_all = 'if'(move_type == 'index', TRUE, FALSE))
}

#' Function calculating windowed quality results for a single RR time series
#' @param RR rr object
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#' @return data.frame with results for windows as rows
#' @export
get_single_quality_windowed_results <- function(RR,
                                                time_functions_list = glb_time_functions,
                                                window_type = "jump",
                                                move_type = "time",
                                                window_length = 5,
                                                cut_end = FALSE,
                                                return_all = FALSE) {
  window_slide = paste(move_type, window_type, sep = "_")
  rr_index <- 'if' (move_type == 'time', 2, 1) # index based windows do not have time track
  time_function <- time_functions_list[[window_slide]]
  lapply('if' (window_type == 'jump', # cut end is only applicable to the jump window type
               time_function(RR, window = window_length, cut_end = cut_end),
               time_function(RR, window = window_length)),
         function(window_table) {
           ret_val <- NULL
           if (move_type == 'index' && nrow(window_table) == window_length) { # TODO - repair this! cut_end does not seem to work for index_based
              ret_val <- hrvhra::describerr(window_table[[rr_index]],
                                            window_table[[rr_index + 1]])
           }
           if (move_type == 'time') {
             ret_val <- hrvhra::describerr(window_table[[rr_index]],
                                           window_table[[rr_index + 1]])
           }
           ret_val
         }) %>%
    dplyr::bind_rows() %>%
    cut_incomplete_rows(cut_end, return_all = 'if'(move_type == 'index', TRUE, FALSE))
}

#' Function adding dynamic asymmetry tests and rounding values
#' @param windowed_results result of a numerical function applied to an RR window
#' @param round_digits how much to round the descirptors
#' @param p_digits how should the p-value be rounded
#' @param asym_comparisons vector of strings, containing comparisons of the form AR1>DR1, SD1d>SD1a etc for use in dynamic asymmetry
round_and_summarize_dynamic_asym <- function(windowed_results, round_digits = 3, p_digits = 4, asym_comparisons = NULL) {
  cols_to_round <- sapply(windowed_results[1, ], is.numeric)
  result <- windowed_results[, cols_to_round] %>%
    colMeans(na.rm = TRUE) %>%
    round(digits = round_digits)
  if (length(result) == 0) {

    to_return <- if (!is.null(asym_comparisons)) {
      comparisons <- get_comparisons_in_windowed_results(windowed_results, asym_comparisons)
      partial_data_frame <- data.frame(t(rep(NA, length(comparisons))))
      names(partial_data_frame) <- comparisons
      cbind(windowed_results[1, ], partial_data_frame)
    } else {
      windowed_results[1, ]
    }
  return(to_return)
  }
  if (!is.data.frame(result)) {
    result <- as.data.frame(t(result))
  }
  if (!is.null(asym_comparisons)) {
    comparisons_in_windowed_results <- get_comparisons_in_windowed_results(windowed_results, asym_comparisons)
    if(length(comparisons_in_windowed_results) == 0) {
      return(result)
    }
    p_s <- c()
    props <- c()
    for (comparison in comparisons_in_windowed_results) {
      vars <- strsplit(comparison, '>')[[1]]
      prop_sum <- sum(windowed_results[[vars[[1]]]] > windowed_results[[vars[[2]]]])
      prop_test <- prop.test(prop_sum, nrow(windowed_results), alternative = 'greater')
      props <- c(props, prop_test$estimate)
      p_s <- c(p_s, puj(prop_test$p.value, rmarkdown = FALSE, digits = p_digits))
    }
    prop_frame <- as.data.frame(t(round(props, round_digits)))
    names(prop_frame) <- paste0(comparisons_in_windowed_results, "_prop")
    pval_frame <- as.data.frame(t(p_s))
    names(pval_frame) <- paste0(comparisons_in_windowed_results, "_pVal")
    result <- cbind(result, prop_frame, pval_frame)
  }
  result
}

#' Function extracting which comparisons can be applied to a window
#'
#' @param windowed_results windowed results to apply the comparisons
#' @param asym_comparisons vector of strings, containing comparisons of the form AR1>DR1, SD1d>SD1a etc for use in dynamic asymmetry
#' @return vector of strings, containing comparisons of the form AR1>DR1, SD1d>SD1a etc for use in dynamic asymmetry
get_comparisons_in_windowed_results <- function(windowed_results, comparisons) {
  sapply(comparisons, function(comparison) {
    vars <- strsplit(comparison, '>')[[1]]
    if (all(vars %in% colnames(windowed_results))) {
      return(comparison)
    } else {
      return(NA)
    }
  }) %>%
    Filter(not_na, .) %>%
    unname()
}

#' Function formatting p-values - copied from my package shiny-tools
#' @param p p value to be formated
#' @param rmarkdown, boolea, whether to use rmarkdown
#' @param digits how much to round the p-value
puj <- function(p, rmarkdown = TRUE, digits) {
  if (rmarkdown ) {
    if (p<0.0001) return ("<em>p</em><0.0001")
    else return(paste("<em>p</em>=", as.character(round(p, digits)), sep=""))
  } else {
    if (p<0.0001) return ("p<0.0001")
    else return(paste0('p=',  as.character(round(p, 4))))
  }
}

#' Function for use in Filter equivalent to !is.na
#' @param x data
not_na <- function(x) !is.na(x)

#' Function to sort out the coluns of the runs results table
#' @param results windowed results
sort_out_runs <- function(results) {
  runs_names <- names(results)
  ARs <- runs_names[grepl('AR', runs_names) & !grepl('_', runs_names)] %>%
    sort_ardrn(type = "AR")
  DRs <- runs_names[grepl('DR', runs_names) & !grepl('_', runs_names)] %>%
    sort_ardrn(type = "DR")
  Ns <- runs_names[grepl('N', runs_names) & !grepl('_', runs_names)] %>%
    sort_ardrn(type = 'N')
  compars_props <- runs_names[grepl('_prop', runs_names)] %>%
    sort_ps(stub = '_prop')
  compars_pvals <- runs_names[grepl('_pVal', runs_names)] %>%
    sort_ps(stub = '_pVal')
  rest <- runs_names[!(runs_names %in% c(DRs, ARs, Ns, "DR_MAX", "AR_MAX", "N_MAX", compars_props, compars_pvals))]
  results[c(rest, ARs, DRs, Ns,  compars_props, compars_pvals)]
}

#' Sorting function for runs
#' @param ardrn_vec vector of runs names
#' @param type DR, AR or N
sort_ardrn <- function(ardrn_vec, type = "DR") {
  ardrn_vec %>%
    sub(pattern = type, replacement = "", .) %>%
    as.numeric() %>%
    sort() %>%
    paste0(type, .)
}

#' Sorting function for comparisons
#' @param ardr_p comparison names
#' @param stub what the stub of the comparison is (_prop or _pVal)
sort_ps <- function(ardr_p, stub = "_prop") {
  if (length(ardr_p) == 0) {
    return(NULL)
  }
  pattern <- paste0("(DR)|(AR)|(", stub, ")")
  names_to_sort <- gsub(pattern = pattern, replacement = "", ardr_p) %>%
    sapply(function(elem) strsplit(elem, split = ">")[[1]][1] %>% as.numeric()) %>%
    sort()
  names(names_to_sort) <- ardr_p
  names(sort(names_to_sort))
}

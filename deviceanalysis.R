# Analyze all Tests from a Device
# CSV SCHEME: Test Repeat;Worker ID;Run ID;Roundtrip;to Worker;from Worker

# Settings
args <- commandArgs(trailingOnly=TRUE)

TEST_DEVICE_PATH <- "mac-mini-m1" # "ipad-a13-bionic" # "windows-i7-8565U" # "linux-i7-8565U"
if (length(args) > 0) TEST_DEVICE_PATH <- args[1]

EVALUATE_TIME_RATIO <- TRUE

EXPORT_RUN_PLOTS <- TRUE
EXPORT_PERFORMANCE_PLOT <- TRUE

CONSIDER_REPEATS <- 5

LOWER_QUANTILE_PROB <- 0.05 # 0.05
UPPER_QUANTILE_PROB <- 0.95 # 0.95

#-------------------------------------------------------------------------------
# Setup

install_if_missing <- function(package) {
  if (!require(package, character.only=TRUE)) {
    install.packages(package, dependencies=TRUE)
  }
  library(package, character.only=TRUE)
}
install_if_missing("data.table")
install_if_missing("stringr")
install_if_missing("pracma")

TEST_DEVICE_PATH_FULL = path.expand(paste(getwd(), TEST_DEVICE_PATH, sep="/"))
PLOT_PATH_FULL = path.expand(paste(getwd(), "graphs", sep="/"))

filter_quantiles <- function(split_data, lower_probability, upper_probability) {
  #split_data <- split(raw_data, raw_data$Run.ID + raw_data$Worker.ID + raw_data$Worker.Amount)
  filtered_list <- list()
  for (id in names(split_data)) {
    subset_data <- split_data[[id]]
    lower_quantile <- quantile(subset_data$Roundtrip, lower_probability)
    upper_quantile <- quantile(subset_data$Roundtrip, upper_probability)
    #IQR <- upper_quantile - lower_quantile
    #lower_bound <- lower_quantile - 1.5 * IQR
    #upper_bound <- upper_quantile + 1.5 * IQR
    #filtered_subset <- subset_data[subset_data$Roundtrip > lower_bound & subset_data$Roundtrip < upper_bound, ]
    filtered_subset <- subset_data
    if (lower_probability > 0) filtered_subset <- filtered_subset[filtered_subset$Roundtrip > lower_quantile, ]
    if (upper_probability < 1) filtered_subset <- filtered_subset[filtered_subset$Roundtrip < upper_quantile, ]
    filtered_list[[id]] <- filtered_subset
  }
  filtered_df <- do.call(rbind, filtered_list)
  rownames(filtered_df) <- NULL
  return(filtered_df)
}

extract_param_from_file_name <- function(string, param) {
  pattern <- paste("(\\d+)", param, sep="")
  matches <- regmatches(string, gregexec(pattern, string))
  return(as.numeric(matches[[1]][2]))
}

extract_object_complexity_from_file_name <- function(string) {
  pattern <- paste("B", "(\\d+)", "D", "(\\d+)", "L", "(\\d+)", sep="")
  matches <- regmatches(string, gregexec(pattern, string))
  return(data.frame(Breadth=c(matches[[1]][2]), Depth=c(matches[[1]][3]), Length=c(matches[[1]][4])))
}

extract_blob_params <- function(file_name) {
  f_type = "B"
  f_size <- extract_param_from_file_name(file_name, "Bytes")
  if (is.na(f_size)) {
    f_type = "KiB"
    f_size <- extract_param_from_file_name(file_name, f_type)
  }
  if (is.na(f_size)) {
    f_type = "MiB"
    f_size <- extract_param_from_file_name(file_name, f_type)
  }
  dispatch_type <- "Shuffled Dispatch"
  if (grepl("SerialDispatch", file_name)) {
    dispatch_type <- "Serial Dispatch"
  }
  res <- data.frame(Value=c(f_size), Unit=c(f_type), Dispatch=c(dispatch_type), Group=factor(paste(f_size, f_type, sep=" "), levels=c("10 B", "100 B", "1 KiB", "10 KiB", "100 KiB", "1 MiB", "10 MiB")) )
  return(res)
}

extract_latency_params <- function(file_name) {
  run_amount <- extract_param_from_file_name(file_name, "Runs")
  dispatch_type <- "Shuffled Dispatch"
  if (grepl("SerialDispatch", file_name)) {
    dispatch_type <- "Serial Dispatch"
  }
  res <- data.frame(Runs=c(run_amount), Dispatch=c(dispatch_type), Group=factor(paste(format(run_amount, scientific=FALSE), "Runs", sep=" ")) ) 
  return(res)
}

extract_object_params <- function(file_name) {
  dispatch_type <- "Shuffled Dispatch"
  if (grepl("SerialDispatch", file_name)) {
    dispatch_type <- "Serial Dispatch"
  }
  key_type <- ""
  if (grepl("DeterministicKeys", file_name)) {
    key_type <- "Deterministic Keys"
  } else if (grepl("RandomKeys", file_name)) {
    key_type <- "Random Keys"
  }
  obj_complexity <- extract_object_complexity_from_file_name(file_name)
  obj_levels = c()
  #for (b in 1:6) {
  for (d in 1:6) {
    level_name <- paste(obj_complexity$Breadth, "Breadth,\n", d, "Depth", sep=" ") 
    obj_levels <- c(obj_levels, c(level_name))
  }
  #}
  res <- data.frame(Dispatch=c(dispatch_type), Keys=c(key_type), Group=factor(paste(obj_complexity$Breadth, "Breadth,\n", obj_complexity$Depth, "Depth", sep=" "), levels=obj_levels) )
  res <- cbind(res, obj_complexity)
  return(res)
}

extract_floats_params <- function(file_name) {
  dispatch_type <- "Shuffled Dispatch"
  if (grepl("SerialDispatch", file_name)) {
    dispatch_type <- "Serial Dispatch"
  }
  key_type <- ""
  if (grepl("DeterministicKeys", file_name)) {
    key_type <- "Deterministic Keys"
  } else if (grepl("RandomKeys", file_name)) {
    key_type <- "Random Keys"
  }
  float_amount <- extract_param_from_file_name(file_name, "Floats")
  res <- data.frame(Dispatch=c(dispatch_type), Keys=c(key_type), Floats=c(float_amount), Group=factor(paste(format(float_amount, scientific=FALSE), "Floats", sep=" ")) )
  return(res)
}

get_test_params <- function(test, file_name) {
  res <- data.frame()
  switch(test,
    "atomicsBlob"={
      res <- extract_blob_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Value, " ", res$Unit, ", ", res$Dispatch, sep="")))
    },
    "postMessageBlobCopy"={
      res <- extract_blob_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Value, " ", res$Unit, ", ", res$Dispatch, sep="")))
    },
    "postMessageBlobTransfer"={
      res <- extract_blob_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Value, " ", res$Unit, ", ", res$Dispatch, sep="")))
    },
    
    "atomicsBinaryObject"={
      res <- extract_object_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Breadth, " Breadth, ", res$Depth, " Depth, ", res$Dispatch, sep="")))
    },
    "postMessageBinaryObject"={
      res <- extract_object_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Breadth, " Breadth, ", res$Depth, " Depth, ", res$Dispatch, sep="")))
    },
    "postMessageObjectCopy"={
      res <- extract_object_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Breadth, " Breadth, ", res$Depth, " Depth, ", res$Dispatch, ", ", res$Keys, sep="")))
    },
    
    "atomicsFloatArray"={
      res <- extract_floats_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(format(res$Floats, scientific=FALSE), " Floats, ", res$Dispatch, sep="")))
    },
    "postMessageFloatArray"={
      res <- extract_floats_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(format(res$Floats, scientific=FALSE), " Floats, ", res$Dispatch, sep="")))
    },
    "postMessageFloatObject"={
      res <- extract_floats_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(format(res$Floats, scientific=FALSE), " Floats, ", res$Dispatch, ", ", res$Keys, sep="")))
    },
    
    "atomicsLatency"={
      res <- extract_latency_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Dispatch, sep="")))
    },
    "postMessageLatency"={
      res <- extract_latency_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Dispatch, sep="")))
    },
    
    "atomicsSpeed"={
      res <- extract_latency_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Dispatch, sep="")))
    },
    "postMessageSpeed"={
      res <- extract_latency_params(file_name)
      res <- cbind(res, data.frame(sub_text=paste(res$Dispatch, sep="")))
    },
  )
  return(res)
}

# ------------------------------------------------------------------------------
# Analysis

# find CSV files in device directory and acquire: test case, file name, full path
csv_files_full_path <- list.files(path=TEST_DEVICE_PATH_FULL, pattern="\\.csv$", full.names=TRUE, recursive=TRUE)
list_of_testfiles <- lapply(csv_files_full_path, function(file_path) {
  file_name <- basename(file_path)
  test_path <- str_sub(file_path, str_length(TEST_DEVICE_PATH_FULL) + 2, str_length(file_path) - str_length(file_name) - 1)
  data <- c(file_path, test_path, file_name)
  return(data)
})

# set test case as list index  
test_types <- split(list_of_testfiles, f=sapply(list_of_testfiles, function(x) x[[2]]))
test_types_col <- names(test_types)

print(paste("Generating Graphs for Device ", TEST_DEVICE_PATH, sep=""))

last_test_category <- ""
test_category_aggregate <- data.frame()

# iterate over all test CSV file paths 
for (tIdx in 1:length(test_types_col)) {
  t <- test_types_col[tIdx]
  tests <- test_types[[t]]
  split_test_attrs <- str_split_1(tests[[1]][2], "/")
  test_browser <- split_test_attrs[1]
  test_category <- split_test_attrs[2]
  test_name <- split_test_attrs[3]
  if (last_test_category != test_category) {
    last_test_category <- test_category
    test_category_aggregate <- data.frame()
  }
  print(paste("Creating Graphs for ", tests[[1]][2], sep=""))
  
  data <- lapply(tests, function(e) {
    csv_data <- read.csv(e[1], sep=";")
    test_browser <- str_split_1(e[2], "/")[1]
    test_name <- str_split_1(e[2], "/")[3]
    workerAmount <- extract_param_from_file_name(e[3], "Workers")
    test_params <- get_test_params(test_name, e[3])
    sub_text <- paste(workerAmount, "Workers,", test_params$sub_text, sep=" ")
    test_params <- test_params[, !(names(test_params) %in% c("sub_text"))]
    csv_data$Worker.Amount <- workerAmount
    csv_data <- cbind(csv_data, test_params)
    # --------------------------------------------------------------------------
    # Plot for Test Runs
    if (EXPORT_RUN_PLOTS) {
      test_data_flat <- csv_data[csv_data$Test.Repeat <= CONSIDER_REPEATS, ]
      # Filter out outliers
      filtered_data <- test_data_flat
      #filtered_data <- filter_quantiles(test_data_flat, LOWER_QUANTILE_PROB, UPPER_QUANTILE_PROB)
      pdf_file_directory <- paste(PLOT_PATH_FULL, TEST_DEVICE_PATH, tests[[1]][2], sep="/")
      pdf_file_name <- paste(str_replace(e[3], ".csv", ""), "_Plot", ".pdf", sep="")
      if (!dir.exists(pdf_file_directory)) {
        dir.create(pdf_file_directory, recursive=TRUE)
      }
      
      split_plot_values <- split(filtered_data, filtered_data$Run.ID + filtered_data$Worker.ID) # + filtered_data$Roundtrip + filtered_data$Worker.Amount + filtered_data$Group
      filtered_data <- filter_quantiles(split_plot_values, LOWER_QUANTILE_PROB, UPPER_QUANTILE_PROB)
      
      mean_roundtrips_repeats <- aggregate(Roundtrip ~ Run.ID + Test.Repeat, data=filtered_data, mean)
      mean_roundtrips_repeats <- mean_roundtrips_repeats[order(mean_roundtrips_repeats$Run.ID), ]
      # offset y-axis so that no negative values occur in logarithmic plot
      #min_value <- min(mean_roundtrips_repeats$Roundtrip)
      #if (min_value < 0) {
      #  mean_roundtrips_repeats$Roundtrip <- mean_roundtrips_repeats$Roundtrip + abs(min_value) #shift
      #}
      #
      mean_roundtrips <- aggregate(Roundtrip ~ Run.ID, data=filtered_data, mean)
      #if (min_value < 0) {
      #  mean_roundtrips$Roundtrip <- mean_roundtrips$Roundtrip + abs(min_value) #shift
      #}
      peak_threshold <- (max(mean_roundtrips$Roundtrip) - min(mean_roundtrips$Roundtrip)) * 0.1 # 10% of the range
      peaks <- findpeaks(mean_roundtrips$Roundtrip, minpeakdistance=25) # , threshold=peak_threshold
      peak_indices <- peaks[, 2]
      peak_indices <- sort(peak_indices)

      plot_values <- mean_roundtrips_repeats
      # graph is logarithmic, only draw values above 0
      plot_values <- plot_values[plot_values$Roundtrip > 0, ]
      
      pdf(file=paste(pdf_file_directory, pdf_file_name, sep="/"), width=8, height=6) # width and height in inches
      if ("Runs" %in% colnames(test_params) && test_params$Runs > 1000) {
        plot(plot_values$Roundtrip ~ plot_values$Run.ID, type="h", 
             xlab="Run", ylab="Roundtrip Duration in Milliseconds", main=paste(TEST_DEVICE_PATH, test_browser, test_name, sep=" "), col="gray30", col.sub="grey20", sub=sub_text, log="y") 
      } else {
        boxplot(plot_values$Roundtrip ~ plot_values$Run.ID, outcex=0.5,
                xlab="Run", ylab="Roundtrip Duration in Milliseconds", main=paste(TEST_DEVICE_PATH, test_browser, test_name, sep=" "), border="gray30", col.sub="gray20", sub=sub_text, log="y")
      }
      grid(nx=NA, ny=NULL, lty=2, col=rgb(0.85, 0.85, 0.85, 0.7), lwd=2) #gray90
      abline(lm(mean_roundtrips$Roundtrip ~ mean_roundtrips$Run.ID, data=mean_roundtrips), col=rgb(1, 0, 0, 0.75), lwd=3, untf=TRUE)
      # draw peaks
      # points(mean_roundtrips$Run.ID[peak_indices], mean_roundtrips$Roundtrip[peak_indices], col=rgb(0, 1, 0, 0.8), pch=19)
      for (i in 1:(length(peak_indices) - 1)) {
        start_idx <- peak_indices[i]
        end_idx <- peak_indices[i + 1]
        if (length(start_idx) == 0 || is.na(start_idx) || length(end_idx) == 0 || is.na(end_idx)) next
        segment_x <- mean_roundtrips$Run.ID[start_idx:end_idx]
        segment_y <- mean_roundtrips$Roundtrip[start_idx:end_idx]
        fit <- lm(segment_y ~ segment_x)
        fitted_values <- predict(fit, newdata=data.frame(segment_x))
        segments(segment_x[1], fitted_values[1], segment_x[length(segment_x)], fitted_values[length(fitted_values)], col=rgb(0, 0, 1, 0.9), lwd=2)
      }
      legend("topleft", legend=c("Global Regression", "Local Regression"), col=c(rgb(1, 0, 0), rgb(0, 0, 1)), lty=c(1, 1), lwd=3, bty="n", pt.cex=2, cex=1, text.col="black", horiz=TRUE, xpd=TRUE, inset=c(0, -0.08)) # pch=19
      dev.off()
    }
    return(csv_data)
  })
  data <- do.call(rbind, data)
  
  data <- data[data$Test.Repeat <= CONSIDER_REPEATS, ]
  # Filter out outliers
  # data <- filter_quantiles(data, LOWER_QUANTILE_PROB, UPPER_QUANTILE_PROB)
  
  data$Time.Ratio <- data$to.Worker / (data$to.Worker + data$from.Worker)

  # Aggregated Worker performance plot
  worker_list <- unique(data$Worker.Amount)
  
  color_range <- c(rgb(5,191,94, maxColorValue=255), rgb(33,87,215, maxColorValue=255), rgb(151,55,204, maxColorValue=255), rgb(198,32,64, maxColorValue=255)) #rgb(14,205,202, maxColorValue=255),
  if (EXPORT_PERFORMANCE_PLOT) {
    pdf_file_directory <- paste(PLOT_PATH_FULL, TEST_DEVICE_PATH, tests[[1]][2], sep="/")
    if (!dir.exists(pdf_file_directory)) {
      dir.create(pdf_file_directory, recursive=TRUE)
    }
    plot_variations <- list(data)
    if (test_name == "postMessageObjectCopy" || test_name == "postMessageBinaryObject" || test_name == "atomicsBinaryObject") {
      plot_variations <- split(data, f=data$Breadth)
    }
    # split dataset into graph variations (deterministic/random keys, as well as serial/shuffled dispatch)
    if ("Keys" %in% colnames(data)) {
      plot_variations <- sapply(plot_variations, function(variation) {
        return(split(variation, variation$Keys))
      })
    }
    plot_variations <- sapply(plot_variations, function(variation) {
      return(split(variation, variation$Dispatch))
    })
    #print(plot_variations)
    pvIdx <- 0
    for (pv in plot_variations) {
      pvIdx = pvIdx + 1
      sub_text <- paste(unique(pv$Dispatch), sep="") 
      if ("Keys" %in% colnames(pv)) {
        unique_keys <- unique(pv$Keys)
        empty_keys <- any(unique_keys == "")
        if (!empty_keys) {
          sub_text <- paste(sub_text, ", ", unique_keys, sep="")
        }
      }
      # histogramm
      unique_groups <- unique(pv$Group)
      for (gIdx in 1:length(as.numeric(unique_groups))) {
        curGroup <- pv[pv$Group == unique_groups[gIdx], ]
        for (wIdx in 1:length(worker_list)) {
          w_1 <- curGroup[curGroup$Worker.Amount == worker_list[wIdx],]
          #w_1[w_1$Time.Ratio < 0 & w_1$Time.Ratio > 1, ]$Time.Ratio <- NA
          #w_1 <- w_1[w_1$Roundtrip > 0, ]
          if (nrow(w_1) > 0) { # plot variation split have their own pv iteration, skip them here
            time_ratio_text <- ""
            if (EVALUATE_TIME_RATIO) {
              valid_ratios <- w_1[!is.nan(w_1$Time.Ratio) & !is.na(w_1$Time.Ratio) & !is.infinite(w_1$Time.Ratio), ] #  & w_1$Time.Ratio >= 0 & w_1$Time.Ratio <= 1
              valid_ratios$Time.Ratio[valid_ratios$Time.Ratio < 0] <- 0
              valid_ratios$Time.Ratio[valid_ratios$Time.Ratio > 1] <- 1
              time_ratio <- median(valid_ratios$Time.Ratio)
              if (!is.nan(time_ratio) && !is.na(time_ratio) && test_browser != "chrome") {
                time_ratio_text <- paste(", Send Ratio ", sprintf("%.2f", time_ratio * 100), "%", sep="")
              }
            }
            pdf(file=paste(pdf_file_directory, paste(TEST_DEVICE_PATH, test_browser, test_name, "Histogramm", "Group", gIdx, paste(worker_list[wIdx], "_Workers", ".pdf", sep=""), sep="_"), sep="/"), width=5.5, height=5) # width and height in inches
            hist(x=w_1$Roundtrip, xlab="", ylab="Occurrence", main=paste(TEST_DEVICE_PATH, test_browser, test_name, sep=" "), col.sub="grey30", sub=paste(worker_list[wIdx], " Workers, ", unique_groups[gIdx], ", ", sub_text, time_ratio_text, sep=""))
            grid(nx=NA, ny=NULL, lty=2, col="gray90", lwd=2)
            title(xlab="Roundtrip Duration in Milliseconds", line=2, cex.lab=1)
            dev.off()
          }
        }
      }
      
      # line graph
      # split_plot_values <- split(pv, pv$Worker.Amount + pv$Worker.ID + as.numeric(pv$Group))
      # pv <- filter_quantiles(split_plot_values, LOWER_QUANTILE_PROB, UPPER_QUANTILE_PROB)
      
      aggregated <- aggregate(Roundtrip ~ Group + Worker.Amount, data=pv, mean)
      yrange <- range(aggregated$Roundtrip)
      xrange <- unique(aggregated$Group) 
      aggregated$Group <- factor(aggregated$Group, levels=xrange)
      graph_width <- max(c(5.5, 1 + (1.2 * length(xrange))))
      
      plot_values <- aggregated
      # graph is logarithmic, only draw values above 0
      plot_values <- plot_values[plot_values$Roundtrip > 0, ]

      time_ratio_text <- ""
      if (EVALUATE_TIME_RATIO) {
        valid_ratios <- pv[!is.nan(pv$Time.Ratio) & !is.na(pv$Time.Ratio) & !is.infinite(pv$Time.Ratio), ] #  & w_1$Time.Ratio >= 0 & w_1$Time.Ratio <= 1
        valid_ratios$Time.Ratio[valid_ratios$Time.Ratio < 0] <- 0
        valid_ratios$Time.Ratio[valid_ratios$Time.Ratio > 1] <- 1
        time_ratios <- aggregate(Time.Ratio ~ Worker.Amount, data=valid_ratios, median)
        time_ratio <- mean(time_ratios$Time.Ratio)
        if (!is.nan(time_ratio) && !is.na(time_ratio) && test_browser != "chrome") {
          sub_text <- paste(sub_text, ", Send Ratio ", sprintf("%.2f", time_ratio * 100), "%", sep="")
        }
      }
      
      pdf(file=paste(pdf_file_directory, paste(TEST_DEVICE_PATH, test_browser, test_name, "Aggregated", "Workers", paste(pvIdx, ".pdf", sep=""), sep="_"), sep="/"), width=graph_width, height=6) # width and height in inches
      plot(x=1, type="n", xlim=c(1,length(xrange)), xaxt="n", ylim=yrange, xlab="", ylab="Average Roundtrip Duration in Milliseconds", main=paste(TEST_DEVICE_PATH, test_browser, test_name, sep=" "), col.sub="grey30", sub=sub_text, log="y") # log="y", xlab="Payload Size",
      grid(nx=NA, ny=NULL, lty=2, col="gray90", lwd=2)
      axis(side=1, labels=xrange, at=c(1:length(xrange)))
      
      for (wIdx in 1:length(worker_list)) {
        w_1 <- plot_values[plot_values$Worker.Amount == worker_list[wIdx],]
        lines(w_1$Roundtrip ~ w_1$Group, type="b", col=color_range[wIdx], lwd=3, pch=19)
      }
      legend("topleft", legend=paste(worker_list, "Workers", sep=" "), col=color_range, pch=19, bty="n", pt.cex=2, cex=1, text.col="black", horiz=TRUE, xpd=TRUE, inset=c(0, -0.08)) # cex=1.2, horiz=FALSE, inset=c(0.02, 0.02)
      dev.off()
    }
    # add data for test category aggregate
    data <- cbind(data, data.frame(Test.Name=test_name))
    test_category_aggregate <- rbind(test_category_aggregate, data)
    
    # CATEGORY: create aggregated plot for each browser that shows all workload types instead of workers 
    category_lines <- c(3, 4, 5)
    category_symbols <- c(1, 0, 2, 23)
    # lookahead to check if test category aggregated view should be created 
    if (tIdx + 1 > length(test_types_col) || str_split_1(test_types_col[tIdx + 1], "/")[2] != test_category) {
      pdf_file_directory <- paste(PLOT_PATH_FULL, TEST_DEVICE_PATH, test_browser, test_category, sep="/")
      plot_variations <- list(test_category_aggregate)
      if (test_name == "postMessageObjectCopy" || test_name == "postMessageBinaryObject" || test_name == "atomicsBinaryObject") {
        plot_variations <- split(test_category_aggregate, f=test_category_aggregate$Breadth)
      }
      # split dataset into graph variations (deterministic/random keys, as well as serial/shuffled dispatch)
      if ("Keys" %in% colnames(test_category_aggregate)) {
        for (pvar in plot_variations) {
          # remove pvar from plot_variations? 
          empty_keys <- pvar[pvar$Keys == "", ]
          content_keys <- pvar[pvar$Keys != "", ]
          content_keys$Test.Name <- paste(content_keys$Test.Name, " (", content_keys$Keys, ")", sep="")
          #plot_variations <- c(list(empty_keys), list(content_keys))
          plot_variations <- c(plot_variations, list(empty_keys), list(content_keys))
        }
        #plot_variations <- sapply(plot_variations, function(variation) {
        #  return(split(variation, variation$Keys))
        #})
      }
      plot_variations <- sapply(plot_variations, function(variation) {
        return(split(variation, variation$Dispatch))
      })
      #print(plot_variations)
      pvIdx <- 0
      for (pv in plot_variations) {
        pvIdx = pvIdx + 1
        category_variations <- unique(pv$Test.Name)
        print(paste("Aggregated Categories: ", paste(category_variations, collapse = ", "), ""))
        sub_text <- paste(unique(pv$Dispatch), sep="") 
        # if ("Keys" %in% colnames(pv)) {
        #   unique_keys <- unique(pv$Keys)
        #   empty_keys <- any(unique_keys == "")
        #   if (!empty_keys) {
        #     sub_text <- paste(sub_text, ", ", unique_keys, sep="")
        #   }
        # }
        
        # split_plot_values <- split(pv, pv$Worker.Amount + pv$Worker.ID + as.numeric(pv$Group))
        # pv <- filter_quantiles(split_plot_values, LOWER_QUANTILE_PROB, 1) # UPPER_QUANTILE_PROB
        
        aggregated <- aggregate(Roundtrip ~ Group + Worker.Amount + Test.Name, data=pv, mean)
        yrange <- range(aggregated$Roundtrip)
        xrange <- unique(aggregated$Group) 
        aggregated$Group <- factor(aggregated$Group, levels=xrange)
        graph_width <- max(c(5.5, 3 + (0.8 * length(xrange))))
        
        plot_values <- aggregated
        # graph is logarithmic, only draw values above 0
        plot_values <- plot_values[plot_values$Roundtrip > 0, ]
        
        options(scipen=999)
        pdf(file=paste(pdf_file_directory, paste(TEST_DEVICE_PATH, test_browser, test_category, "Aggregated", "Category", paste(pvIdx, ".pdf", sep=""), sep="_"), sep="/"), width=graph_width, height=6) # width and height in inches
        plot(x=1, type="n", xlim=c(1,length(xrange)), xaxt="n", ylim=yrange, xlab="", ylab="Average Roundtrip Duration in Milliseconds", main=paste(TEST_DEVICE_PATH, test_browser, test_category, sep=" "), col.sub="grey30", sub=sub_text, log="y") # log="y", xlab="Payload Size",
        grid(nx=NA, ny=NULL, lty=2, col="gray90", lwd=2)
        axis(side=1, labels=xrange, at=c(1:length(xrange)))
        for (tcIdx in 1:length(category_variations)) {
          tc_1 <- plot_values[plot_values$Test.Name == category_variations[tcIdx],]
          for (wIdx in 1:length(worker_list)) {
            w_1 <- tc_1[tc_1$Worker.Amount == worker_list[wIdx],]
            lines(w_1$Roundtrip ~ w_1$Group, type="b", col=color_range[wIdx], lty=category_lines[tcIdx] , lwd=2, pch=category_symbols[tcIdx])
          }
        }
        legend("topleft", legend=paste(worker_list, "Workers", sep=" "), col=color_range, pch=19, bty="n", pt.cex=1.5, cex=1, text.col="black", horiz=TRUE, xpd=TRUE, inset=c(0, -0.08)) # cex=1.2, horiz=FALSE, inset=c(0.02, 0.02)
        legend("topleft", legend=paste(category_variations, "     ", sep=""), lty=category_lines, bty="n", pch=category_symbols, lwd=2, pt.cex=1.5, cex=0.8, text.col=rgb(0,0,0, 0.8), horiz=FALSE, xpd=TRUE, inset=c(0, 0.0), x.intersp=0.8, text.width=NA) # cex=1.2, horiz=FALSE, inset=c(0.02, 0.02)
        dev.off()
        options(scipen=0)
      }
    }
    
  }
}
print("Finished creating Graphs")


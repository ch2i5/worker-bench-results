#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <test_device_folder>" #  <plot_title>
    exit 1
fi

# Assign command-line arguments to variables
TEST_DEVICE_FOLDER="$1"
#PLOT_TITLE="$2"

# Run the R script with parameters
Rscript deviceanalysis.R "$TEST_DEVICE_FOLDER" #"$PLOT_TITLE"

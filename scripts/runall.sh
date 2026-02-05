#!/bin/bash
#===============================================================================
# run_all.sh
# MA424 Summative Project - R Script Runner
# Group: 61115, 68729, 70320
#
# This script runs all R scripts for the project in order.
# Usage: ./run_all.sh
#===============================================================================

echo "========================================"
echo "MA424 Summative Project - Running All R Scripts"
echo "Group: 61115, 68729, 70320"
echo "========================================"
echo ""

# Set the directory containing the R scripts
# Modify this path if your scripts are in a different location
SCRIPT_DIR="../Code"

# Check if the Code directory exists, otherwise use current directory
if [ ! -d "$SCRIPT_DIR" ]; then
    SCRIPT_DIR="."
    echo "Note: 'Code' directory not found, using current directory."
    echo ""
fi

# Function to run an R script and check for errors
run_script() {
    local script_name=$1
    local script_path="$SCRIPT_DIR/$script_name"
    
    if [ -f "$script_path" ]; then
        echo "----------------------------------------"
        echo "Running: $script_name"
        echo "----------------------------------------"
        Rscript "$script_path"
        
        if [ $? -eq 0 ]; then
            echo "✓ $script_name completed successfully."
        else
            echo "✗ ERROR: $script_name failed!"
            echo "  Check the script for errors."
        fi
        echo ""
    else
        echo "⚠ WARNING: $script_name not found at $script_path"
        echo ""
    fi
}

# Run all scripts in order by question

echo "========== QUESTION 1 =========="
run_script "Q1i.R"
run_script "Q1ii.R"
run_script "Q1iii.R"
run_script "Q1iv.R"

echo "========== QUESTION 2 =========="
run_script "Q2.R"

echo "========== QUESTION 3 =========="
run_script "Q3i.R"
run_script "Q3ii.R"

echo "========== QUESTION 4 =========="
run_script "Q4i.R"
run_script "Q4ii.R"

echo "========================================"
echo "All scripts have been processed."
echo "========================================"
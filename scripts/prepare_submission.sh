#!/bin/bash
#===============================================================================
# prepare_submission.sh
# MA424 Summative Project - Submission Preparation Script
# Group: 61115, 68729, 70320
#
# This script:
# 1. Renames files according to submission requirements
# 2. Creates the properly named zip file for submission
#
# Usage: ./prepare_submission.sh
#
# IMPORTANT: Run this script from the scripts directory
# Expected structure:
#   ../Code/                       - Contains R scripts
#   ../Report/                     - Contains the PDF report
#   ./                             - Contains group_dynamics_*.pdf files
#===============================================================================

# Group examination numbers (sorted numerically for consistency)
EXAM_1="61115"
EXAM_2="68729"
EXAM_3="70320"

# Submission file name (examination numbers separated by underscores)
SUBMISSION_NAME="${EXAM_1}_${EXAM_2}_${EXAM_3}"

echo "========================================"
echo "MA424 Submission Preparation Script"
echo "Group: $SUBMISSION_NAME"
echo "========================================"
echo ""

# Create a temporary directory for organizing submission files
TEMP_DIR="submission_temp"
rm -rf "$TEMP_DIR"
mkdir -p "$TEMP_DIR"

echo "Step 1: Copying and renaming R scripts..."
echo "----------------------------------------"

# Define source directory for R scripts
# Script is run from scripts/, so Code is at ../Code
CODE_DIR="../Code"
if [ ! -d "$CODE_DIR" ]; then
    echo "  ⚠ Code directory not found at $CODE_DIR"
    CODE_DIR="."
fi

# Expected R script names
declare -a SCRIPTS=(
    "Q1i.R"
    "Q1ii.R"
    "Q1iii.R"
    "Q1iv.R"
    "Q3i.R"
    "Q3ii.R"
    "Q4i.R"
    "Q4ii.R"
)

# Function to find and copy R script
copy_r_script() {
    local target_name=$1
    local found=0
    
    # First, try exact match in Code directory
    if [ -f "$CODE_DIR/$target_name" ]; then
        cp "$CODE_DIR/$target_name" "$TEMP_DIR/$target_name"
        echo "  ✓ Copied $target_name"
        found=1
    fi
    
    # If not found, search in current directory
    if [ $found -eq 0 ] && [ -f "./$target_name" ]; then
        cp "./$target_name" "$TEMP_DIR/$target_name"
        echo "  ✓ Copied $target_name (from current dir)"
        found=1
    fi
    
    # If still not found, try case-insensitive search
    if [ $found -eq 0 ]; then
        local lower_name=$(echo "$target_name" | tr '[:upper:]' '[:lower:]')
        for f in "$CODE_DIR"/*.R "$CODE_DIR"/*.r ./*.R ./*.r; do
            if [ -f "$f" ]; then
                local base=$(basename "$f")
                local base_lower=$(echo "$base" | tr '[:upper:]' '[:lower:]')
                if [ "$base_lower" == "$lower_name" ]; then
                    cp "$f" "$TEMP_DIR/$target_name"
                    echo "  ✓ Copied $base -> $target_name"
                    found=1
                    break
                fi
            fi
        done
    fi
    
    if [ $found -eq 0 ]; then
        echo "  ⚠ WARNING: $target_name not found"
    fi
}

# Copy all R scripts
for script in "${SCRIPTS[@]}"; do
    copy_r_script "$script"
done

echo ""
echo "Step 2: Copying and renaming the report PDF..."
echo "----------------------------------------"

# Find and copy the report PDF
REPORT_FOUND=0
REPORT_TARGET="${SUBMISSION_NAME}.pdf"

# Search locations for the report
# Script runs from scripts/, so Report folder is at ../Report
REPORT_DIR="../Report"

# First, try to find the correctly named report or common LaTeX output names
for name in "${SUBMISSION_NAME}.pdf" "main.pdf" "report.pdf" "Report.pdf" "MA424_report.pdf"; do
    if [ -f "$REPORT_DIR/$name" ]; then
        cp "$REPORT_DIR/$name" "$TEMP_DIR/$REPORT_TARGET"
        echo "  ✓ Copied $name -> $REPORT_TARGET"
        REPORT_FOUND=1
        break
    fi
done

# If not found, find the largest PDF in Report folder (excluding group_dynamics)
if [ $REPORT_FOUND -eq 0 ]; then
    echo "  Looking for largest PDF file..."
    LARGEST_PDF=""
    LARGEST_SIZE=0
    
    for pdf in "$REPORT_DIR"/*.pdf; do
        if [ -f "$pdf" ]; then
            base=$(basename "$pdf")
            # Skip group_dynamics files, figure files, and target file
            if [[ "$base" != group_dynamics* ]] && [[ "$base" != "$REPORT_TARGET" ]] && [[ "$base" != fig* ]] && [[ "$base" != figure* ]]; then
                size=$(stat -f%z "$pdf" 2>/dev/null || stat -c%s "$pdf" 2>/dev/null)
                if [ "$size" -gt "$LARGEST_SIZE" ]; then
                    LARGEST_SIZE=$size
                    LARGEST_PDF=$pdf
                fi
            fi
        fi
    done
    
    if [ -n "$LARGEST_PDF" ]; then
        cp "$LARGEST_PDF" "$TEMP_DIR/$REPORT_TARGET"
        echo "  ✓ Copied $(basename $LARGEST_PDF) -> $REPORT_TARGET (largest PDF: ${LARGEST_SIZE} bytes)"
        REPORT_FOUND=1
    fi
fi

if [ $REPORT_FOUND -eq 0 ]; then
    echo "  ⚠ WARNING: Report PDF not found!"
    echo "    Searched in: $REPORT_DIR"
    echo "    Looking for: main.pdf, report.pdf, or largest PDF"
    echo "    Please manually add your report as: $REPORT_TARGET"
fi

echo ""
echo "Step 3: Copying group dynamics files..."
echo "----------------------------------------"

# Copy group dynamics files
# They should be in current directory (scripts/)
GD_LOCATIONS=(
    "."
    "../Report"
)

for exam_num in $EXAM_1 $EXAM_2 $EXAM_3; do
    GD_FOUND=0
    GD_TARGET="group_dynamics_${exam_num}.pdf"
    
    for loc in "${GD_LOCATIONS[@]}"; do
        if [ -f "$loc/$GD_TARGET" ]; then
            cp "$loc/$GD_TARGET" "$TEMP_DIR/$GD_TARGET"
            echo "  ✓ Copied $GD_TARGET"
            GD_FOUND=1
            break
        fi
        # Also try without underscore
        if [ -f "$loc/group_dynamics${exam_num}.pdf" ]; then
            cp "$loc/group_dynamics${exam_num}.pdf" "$TEMP_DIR/$GD_TARGET"
            echo "  ✓ Copied group_dynamics${exam_num}.pdf -> $GD_TARGET"
            GD_FOUND=1
            break
        fi
    done
    
    if [ $GD_FOUND -eq 0 ]; then
        echo "  ⚠ WARNING: $GD_TARGET not found!"
    fi
done

echo ""
echo "Step 4: Creating the submission zip file..."
echo "----------------------------------------"

# Remove old zip if exists
ZIP_NAME="${SUBMISSION_NAME}.zip"
rm -f "$ZIP_NAME"

# Create the zip file
cd "$TEMP_DIR"
zip -r "../$ZIP_NAME" ./*
cd ..

if [ -f "$ZIP_NAME" ]; then
    echo "  ✓ Created $ZIP_NAME"
else
    echo "  ✗ ERROR: Failed to create zip file!"
fi

echo ""
echo "Step 5: Verification..."
echo "----------------------------------------"

# List contents of the zip
echo "Contents of $ZIP_NAME:"
unzip -l "$ZIP_NAME"

echo ""
echo "========================================"
echo "Submission Checklist:"
echo "========================================"

# Check all required files
echo ""
echo "Required files:"

# Check report
if unzip -l "$ZIP_NAME" | grep -q "$REPORT_TARGET"; then
    echo "  ✓ Report: $REPORT_TARGET"
else
    echo "  ✗ MISSING: Report ($REPORT_TARGET)"
fi

# Check R scripts
for script in "${SCRIPTS[@]}"; do
    if unzip -l "$ZIP_NAME" | grep -q "$script"; then
        echo "  ✓ R Script: $script"
    else
        echo "  ✗ MISSING: $script"
    fi
done

# Check group dynamics
for exam_num in $EXAM_1 $EXAM_2 $EXAM_3; do
    GD_FILE="group_dynamics_${exam_num}.pdf"
    if unzip -l "$ZIP_NAME" | grep -q "$GD_FILE"; then
        echo "  ✓ Group Dynamics: $GD_FILE"
    else
        echo "  ✗ MISSING: $GD_FILE"
    fi
done

echo ""
echo "========================================"
echo "IMPORTANT REMINDERS:"
echo "========================================"
echo "1. Verify the report is max 8 pages, 11pt font, single spacing"
echo "2. Verify all R scripts start with set.seed(1)"
echo "3. Verify all R scripts only use runif(n) for random variables"
echo "4. Verify all R scripts have detailed comments"
echo "5. Only ONE group member should upload to Moodle"
echo "6. All members should be present during upload"
echo ""
echo "Submission file ready: $ZIP_NAME"
echo "========================================"

# Cleanup
rm -rf "$TEMP_DIR"
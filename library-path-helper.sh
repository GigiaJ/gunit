#!/bin/bash

# Define the program and the expected substring in the output
PROGRAM="PROGRAM TO RUN"
EXPECTED_OUTPUT="SUBSTRING TO CHECK FOR"

# Supply the initial LD_LIBRARY_PATH as a variable
INITIAL_LD_LIBRARY_PATH="$1"

# Check if the variable is supplied
if [ -z "$INITIAL_LD_LIBRARY_PATH" ]; then
    echo "Error: Please provide the initial LD_LIBRARY_PATH as the first argument."
    exit 1
fi

# Read the supplied LD_LIBRARY_PATH into an array
IFS=':' read -ra LIB_PATHS <<< "$INITIAL_LD_LIBRARY_PATH"

# Function to generate combinations of a specific size
combinations() {
    local size=$1
    local prefix=$2
    local start_index=${3:-0}
    local remaining=("${@:4}")
    local count=${#remaining[@]}

    if [ $size -eq 0 ]; then
        # Export the current combination of LD_LIBRARY_PATH
        export LD_LIBRARY_PATH="$prefix"

        # Start the program and capture its output
        echo "Trying with LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
        OUTPUT=$("$PROGRAM" 2>&1)

        # Check if the output contains the expected substring
        if [[ "$OUTPUT" == *"$EXPECTED_OUTPUT"* ]]; then
            echo "Success! Output contains the expected substring with LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
            exit 0
        else
            echo "Output does not contain the expected substring."
        fi
    else
        for ((i = start_index; i < count; i++)); do
            local next_prefix="${prefix}:${remaining[i]}"
            local next_remaining=("${remaining[@]:0:i}" "${remaining[@]:i+1}")
            combinations $((size - 1)) "${next_prefix#:}" $((i + 1)) "${remaining[@]}"
        done
    fi
}

# Generate combinations incrementally by size
for size in $(seq 1 ${#LIB_PATHS[@]}); do
    echo "Trying combinations of size $size..."
    combinations $size "" 0 "${LIB_PATHS[@]}"
done

echo "All combinations attempted, no match found."
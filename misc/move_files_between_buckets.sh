#!/bin/bash

# --- CONFIGURATION ---
SOURCE_BUCKET="gs://"
DEST_BUCKET="gs://"

# --- 1. LOGIN ---
echo "Starting Google Cloud authentication..."
# 'gcloud auth login' is for the CLI tool itself
gcloud auth login --no-launch-browser

# --- 2. EXECUTE MOVE ---
echo "Moving data from $SOURCE_BUCKET to $DEST_BUCKET..."

# Using 'mv' handles copy + delete in one go. "cp" just copies.  
# Since it's bucket-to-bucket, it's a server-side operation.
gcloud storage cp --recursive "$SOURCE_BUCKET"/* "$DEST_BUCKET"


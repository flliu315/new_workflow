#!/bin/bash

echo "Running data cleaning..."
Rscript src/01_clean.R

echo "Running analysis..."
Rscript src/02_analysis.R

echo "Pipeline finished."
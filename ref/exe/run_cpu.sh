#!/bin/sh

export CUDA_VISIBLE_DEVICES="0"

echo
echo "Running on cpu"
echo "======================="
./crtm_kernel_cpu

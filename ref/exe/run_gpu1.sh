#!/bin/sh

export CUDA_VISIBLE_DEVICES="0"

echo
echo "Running version 1 of GPU kernels"
echo "================================="
PGI_COMPARE=abs=13 ./crtm_kernel_gpu_v1

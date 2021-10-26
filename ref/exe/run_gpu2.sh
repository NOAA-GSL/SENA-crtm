#!/bin/sh

export CUDA_VISIBLE_DEVICES="0"

echo
echo "Running version 2 of GPU kernel"
echo "==============================="
PGI_COMPARE=abs=13 ./crtm_kernel_gpu_v2

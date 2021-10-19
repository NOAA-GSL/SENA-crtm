list( APPEND crtm_kernel_cpu_src_files
kernel.F90
)

list( APPEND crtm_kernel_gpu_v1_src_files
kernel_v1.F90
)

list( APPEND crtm_kernel_gpu_v2_src_files
kernel_v2.F90
)

add_executable( crtm_kernel_cpu ${crtm_kernel_cpu_src_files} )
if(ENABLE_GPU)
  add_executable( crtm_kernel_gpu_v1 ${crtm_kernel_gpu_v1_src_files} )
  add_executable( crtm_kernel_gpu_v2 ${crtm_kernel_gpu_v2_src_files} )
  target_compile_options(crtm_kernel_gpu_v1 PUBLIC ${OpenACC_Fortran_OPTIONS},cuda${CUDAToolkit_VERSION_MAJOR}.${CUDAToolkit_VERSION_MINOR})
  target_compile_options(crtm_kernel_gpu_v2 PUBLIC ${OpenACC_Fortran_OPTIONS},cuda${CUDAToolkit_VERSION_MAJOR}.${CUDAToolkit_VERSION_MINOR})
endif()

if(OpenMP_FOUND)
    target_link_libraries(crtm_kernel_cpu PUBLIC OpenMP::OpenMP_Fortran)
    if(ENABLE_GPU)
      target_link_libraries(crtm_kernel_gpu_v1 PUBLIC OpenMP::OpenMP_Fortran)
      target_link_libraries(crtm_kernel_gpu_v2 PUBLIC OpenMP::OpenMP_Fortran)
      target_link_options(crtm_kernel_gpu_v1 PUBLIC ${OpenACC_Fortran_OPTIONS})
      target_link_options(crtm_kernel_gpu_v2 PUBLIC ${OpenACC_Fortran_OPTIONS})
    endif()
endif()

install(TARGETS crtm_kernel_cpu
        DESTINATION ${PROJECT_SOURCE_DIR}/exe)
if(ENABLE_GPU)
 install(TARGETS crtm_kernel_gpu_v1
	 DESTINATION ${PROJECT_SOURCE_DIR}/exe)
 install(TARGETS crtm_kernel_gpu_v2
	 DESTINATION ${PROJECT_SOURCE_DIR}/exe)
endif()

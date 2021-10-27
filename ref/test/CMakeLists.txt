list( APPEND crtm_test_output
  test_output/output.test
)
 
add_subdirectory( tools )

# Create test_output directory for test output and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test_output)
foreach(FILENAME ${crtm_test_output})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/../../test/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach()

set(COMPARE_SH ${CMAKE_BINARY_DIR}/bin/crtm_compare.sh)

# Add unit tests here.

# CPU kernel
add_test(NAME cpu_kernel
         COMMAND bash -c "../src/crtm_kernel_cpu > test_output/crtm_kernel_cpu.log")
add_test(NAME compare_cpu_kernel
           COMMAND ${COMPARE_SH} test_output/crtm_kernel_cpu.log test_output/output.test)

# Tests when GPU is enabled
if ( ENABLE_GPU )

  # GPU kernel version 1
  add_test(NAME gpu_kernel_v1
           COMMAND bash -c "../src/crtm_kernel_gpu_v1 > test_output/crtm_kernel_gpu_v1.log")
  add_test(NAME compare_gpu_kernel_v1
           COMMAND ${COMPARE_SH} test_output/crtm_kernel_gpu_v1.log test_output/output.test)

  # GPU kernel version 2
  add_test(NAME gpu_kernel_v2
           COMMAND bash -c "../src/crtm_kernel_gpu_v2 > test_output/crtm_kernel_gpu_v2.log")
  add_test(NAME compare_gpu_kernel_v2
           COMMAND ${COMPARE_SH} test_output/crtm_kernel_gpu_v2.log test_output/output.test)

endif()



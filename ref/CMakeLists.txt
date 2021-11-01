################################################################################
# crtm
################################################################################

cmake_minimum_required( VERSION 3.10 )

project( crtm LANGUAGES C Fortran )

list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

option( ENABLE_GPU "Build to run on GPU" OFF )

enable_testing()

include(${PROJECT_NAME}_compiler_flags)

find_package( OpenMP COMPONENTS C Fortran )

if ( ENABLE_GPU )
  set( OpenACC_ACCEL_TARGET tesla )
  find_package( OpenACC REQUIRED )
  find_package( CUDAToolkit REQUIRED )
  add_compile_definitions(ENABLE_GPU)
endif()

add_subdirectory(src)
add_subdirectory(test)


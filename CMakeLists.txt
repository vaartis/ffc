cmake_minimum_required(VERSION 3.2)
project(FF C CXX)

include(ExternalProject)

option(test "Build tests" OFF)

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "6.0.0")
    message(FATAL_ERROR "At least GCC 6 is required")
  endif()
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.0.0")
    message(FATAL_ERROR "At least Clang 4 is required")
  endif()
  add_definitions(-Wimplicit-fallthrough)
endif()

find_package(LLVM 4 REQUIRED)
message(STATUS "Found LLVM: ${LLVM_INSTALL_PREFIX}")

ExternalProject_Add(Variant
  GIT_REPOSITORY "https://github.com/mpark/variant"
  GIT_TAG "v1.0.0"
  PREFIX "${CMAKE_SOURCE_DIR}/deps/download/variant"

  UPDATE_COMMAND ""
  CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=${CMAKE_SOURCE_DIR}/deps/variant")

set(DEPS Variant) # External dependencies

include_directories(
  "include"
  "deps/variant/include"
  ${LLVM_INCLUDE_DIRS}
  )

add_definitions(
  -std=c++1z
  ${LLVM_DEFINITIONS}
  )

if(test)
  find_package(GTest REQUIRED)

  file(GLOB tests_SRC "test/*/*.cpp")

  add_executable(run_tests ${tests_SRC} "src/ASTParser.cpp" "src/TokenStream.cpp")
  add_dependencies(run_tests ${DEPS})

  include_directories(${GTEST_INCLUDE_DIRS})
  target_link_libraries(run_tests ${GTEST_BOTH_LIBRARIES} pthread)
else()
  file(GLOB ff_SRC "src/*.cpp")

  add_executable(ffc ${ff_SRC})
  add_dependencies(ffc ${DEPS})

  llvm_map_components_to_libnames(llvm_libs all)

  target_link_libraries (ffc ${llvm_libs})
endif()

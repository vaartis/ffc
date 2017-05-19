cmake_minimum_required(VERSION 3.2)
project(FF C CXX)

include(ExternalProject)
include(CheckCXXSourceCompiles)

option(test "Build tests" OFF)

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "7.0.0")
    message(FATAL_ERROR "At least GCC 7 is required")
  endif()
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.0.0")
    message(FATAL_ERROR "At least Clang 4 is required")
  endif()
  add_definitions(-Wimplicit-fallthrough)
endif()

set(CMAKE_REQUIRED_FLAGS "-std=c++1z")
CHECK_CXX_SOURCE_COMPILES("#include <optional>\n\rint main() { return 0; }" HAVE_OPTIONAL)
if (HAVE_OPTIONAL)
  add_definitions(-DHAVE_OPTIONAL)
endif()

find_program(LLVM_CONFIG llvm-config)
execute_process(COMMAND ${LLVM_CONFIG} --includedir OUTPUT_VARIABLE LLVM_INCLUDE OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${LLVM_CONFIG} --libs OUTPUT_VARIABLE LLVM_LIBS OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${LLVM_CONFIG} --libdir OUTPUT_VARIABLE LLVM_LIBDIR OUTPUT_STRIP_TRAILING_WHITESPACE)

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
  "${LLVM_INCLUDE}"
  )
link_directories(
  "${LLVM_LIBDIR}"
  )

add_definitions("${LLVM_CXX}" -g -std=c++1z)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)

if(test)
  ExternalProject_Add(GTest
    GIT_REPOSITORY "https://github.com/google/googletest"

    PREFIX "${CMAKE_SOURCE_DIR}/deps/download/gtest"

    UPDATE_COMMAND ""
    CMAKE_ARGS "-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}" "-DBUILD_GMOCK=OFF" "-DBUILD_GTEST=ON" "-DCMAKE_INSTALL_PREFIX=${CMAKE_SOURCE_DIR}/deps/gtest")


  list(APPEND DEPS GTest)

  message(STATUS "Tests are enabled")

  include_directories("deps/gtest/include")
  link_directories("deps/gtest/lib")

  file(GLOB tests_SRC "test/*/*.cpp")
  list(APPEND tests_SRC "src/ASTParser.cpp" "src/TokenStream.cpp" "src/CodeGen.cpp")

  add_executable(run_tests ${tests_SRC})
  add_dependencies(run_tests ${DEPS})

  target_link_libraries(run_tests "${LLVM_LIBS}" gtest gtest_main pthread)
else()
  file(GLOB ff_SRC "src/*.cpp")

  add_executable(ffc ${ff_SRC})
  add_dependencies(ffc ${DEPS})
  target_link_libraries(ffc ${LLVM_LIBS})
endif()

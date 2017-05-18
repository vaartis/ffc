cmake_minimum_required(VERSION 3.2)
project(FF C CXX)

include(ExternalProject)
include(CheckCXXSourceCompiles)

option(test "Build tests" OFF)

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "7.0.0")
    message(FATAL_ERROR "At least GCC 7 is required")
  endif()
  add_definitions(-std=c++17)
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.0.0")
    message(FATAL_ERROR "At least Clang 4 is required")
  endif()
  add_definitions(-Wimplicit-fallthrough -std=c++1z)
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
  "${LLVM_LIBIR}"
  )

add_definitions("${LLVM_CXX}" -g)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)

CHECK_CXX_SOURCE_COMPILES("#include <optional>\n\rint main() { return 0; }" HAVE_OPTIONAL)

if(test)
  message(STATUS "Tests are enabled")
  find_program(GTEST_CONFIG gtest-config)
  execute_process(COMMAND ${GTEST_CONFIG} --includedir OUTPUT_VARIABLE GTEST_INCLUDE OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${GTEST_CONFIG} --libs OUTPUT_VARIABLE GTEST_LIBS OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${GTEST_CONFIG} --libdir OUTPUT_VARIABLE GTEST_LIBDIR OUTPUT_STRIP_TRAILING_WHITESPACE)

  include_directories("${GTEST_INCLUDE}")
  link_directories("${GTEST_LIBDIR}")

  file(GLOB tests_SRC "test/*/*.cpp")
  list(APPEND tests_SRC "src/ASTParser.cpp" "src/TokenStream.cpp" "src/CodeGen.cpp")

  add_executable(run_tests ${tests_SRC})
  add_dependencies(run_tests ${DEPS})

  include_directories(${GTEST_INCLUDE_DIRS})
  target_link_libraries(run_tests "${GTEST_LIBS}" gtest_main "${LLVM_LIBS}" pthread)
else()
  file(GLOB ff_SRC "src/*.cpp")

  add_executable(ffc ${ff_SRC})
  add_dependencies(ffc ${DEPS})
  target_link_libraries(ffc ${LLVM_LIBS})
endif()

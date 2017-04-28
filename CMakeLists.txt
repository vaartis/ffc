cmake_minimum_required(VERSION 3.2)
set(CMAKE_CXX_COMPILER clang++-4.0)
project(FF C CXX)

include_directories("include")

option(test "Build tests" OFF)

set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "-stdlib=libc++ -std=c++1z -Wimplicit-fallthrough")

include(ExternalProject)

ExternalProject_Add(Variant
  GIT_REPOSITORY "https://github.com/mpark/variant"
  GIT_TAG "v1.0.0"
  PREFIX "${CMAKE_SOURCE_DIR}/deps/download/variant"

  UPDATE_COMMAND ""
  CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=${CMAKE_SOURCE_DIR}/deps/variant")

include_directories("${CMAKE_SOURCE_DIR}/deps/variant/include")
set(DEPS Variant)

if(test)
  find_package(GTest REQUIRED)

  file(GLOB tests_SRC "test/*/*.cpp")

  add_executable(run_tests ${tests_SRC} "src/ASTParser.cpp" "src/TokenStream.cpp")
  add_dependencies(run_tests ${DEPS})

  include_directories(${GTEST_INCLUDE_DIRS})
  target_link_libraries(run_tests ${GTEST_BOTH_LIBRARIES})
else()
  file(GLOB ff_SRC "src/*.cpp")

  add_executable(ffc ${ff_SRC})
  add_dependencies(ffc ${DEPS})

  find_package(LLVM REQUIRED CONFIG)
  include_directories(${LLVM_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS})

  llvm_map_components_to_libnames(llvm_libs all)

  target_link_libraries (ffc ${llvm_libs})
endif()

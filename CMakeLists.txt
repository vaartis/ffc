cmake_minimum_required(VERSION 3.2)
set(CMAKE_CXX_COMPILER clang++)
project(FF CXX)

include_directories("include")

option(test "Build tests" OFF)

set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "-std=c++1z -g -Wimplicit-fallthrough")

if(test)
  find_package(GTest REQUIRED)

  file(GLOB tests_SRC "test/*.cpp")

  add_executable(run_tests ${tests_SRC} "src/ASTParser.cpp" "src/TokenStream.cpp")

  include_directories(${GTEST_INCLUDE_DIRS})
  target_link_libraries(run_tests ${GTEST_BOTH_LIBRARIES})
else()
  file(GLOB ff_SRC "src/*.cpp")

  add_executable(ffc ${ff_SRC})

  find_package(LLVM REQUIRED CONFIG)
  include_directories(${LLVM_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS})

  llvm_map_components_to_libnames(llvm_libs all)

  target_link_libraries (ffc ${llvm_libs})
endif()

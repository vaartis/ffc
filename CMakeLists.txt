cmake_minimum_required(VERSION 3.2)
project(FF CXX)
set(CMAKE_CXX_COMPILER clang++)

file(GLOB ff_SRC "src/*.cpp")

include_directories("include")

add_executable(ff ${ff_SRC})

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "-std=c++14 -g")

llvm_map_components_to_libnames(llvm_libs support core)

target_link_libraries (ff ${llvm_libs})

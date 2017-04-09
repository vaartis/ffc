cmake_minimum_required(VERSION 3.2)
set(CMAKE_CXX_COMPILER clang++)
project(FF CXX)

file(GLOB ff_SRC "src/*.cpp")

include_directories("include")

add_executable(ffc ${ff_SRC})

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "-std=c++1z -g")

llvm_map_components_to_libnames(llvm_libs all)

target_link_libraries (ffc ${llvm_libs})

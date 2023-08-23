cmake_minimum_required(VERSION 3.20)
project(nanorhiwidgetmini LANGUAGES C CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 COMPONENTS Core Gui Widgets ShaderTools)

qt_add_executable(nanorhiwidgetmini
    nanorhiwidgetmini.cpp
)

set(nanovg_base ../../nanovg)
set(nanovg_target nanorhiwidgetmini)
include(${nanovg_base}/nanovg.cmakeinc)

target_link_libraries(nanorhiwidgetmini PUBLIC
    Qt::Core
    Qt::GuiPrivate
    Qt::Widgets
)

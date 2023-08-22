cmake_minimum_required(VERSION 3.20)
project(nanowindow LANGUAGES C CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 COMPONENTS Core)
find_package(Qt6 COMPONENTS Gui)
find_package(Qt6 COMPONENTS ShaderTools)

qt_add_executable(nanowindow
    nanowindow.cpp
)

target_link_libraries(nanowindow PUBLIC
    Qt::Core
    Qt::GuiPrivate
)

set(nanovg_base ../../nanovg)
set(nanovg_target nanowindow)
include(${nanovg_base}/nanovg.cmakeinc)

qt6_add_shaders(nanowindow "shaders"
    PREFIX
        "/shaders"
    FILES
        color.vert
        color.frag
)

qt6_add_resources(nanowindow "resources"
    PREFIX
        "/"
    BASE
        "../shared"
    FILES
        "../shared/fonts/RobotoMono-Medium.ttf"
)

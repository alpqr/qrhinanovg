cmake_minimum_required(VERSION 3.20)
project(nanoinwindow LANGUAGES C CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 COMPONENTS Core)
find_package(Qt6 COMPONENTS Gui)
find_package(Qt6 COMPONENTS ShaderTools)

qt_add_executable(nanoinwindow
    nanoinwindow.cpp
)

target_link_libraries(nanoinwindow PUBLIC
    Qt::Core
    Qt::GuiPrivate
)

set(nanovg_base ../../nanovg)
set(nanovg_target nanoinwindow)
include(${nanovg_base}/nanovg.cmakeinc)

qt6_add_shaders(nanoinwindow "shaders"
    PREFIX
        "/shaders"
    FILES
        color.vert
        color.frag
)

qt6_add_resources(nanoinwindow "resources"
    PREFIX
        "/"
    BASE
        "../shared"
    FILES
        "../shared/fonts/RobotoMono-Medium.ttf"
)

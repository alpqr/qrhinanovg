cmake_minimum_required(VERSION 3.20)
project(nanorhiwidget LANGUAGES C CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 COMPONENTS Core Gui Widgets ShaderTools)

qt_add_executable(nanorhiwidget
    nanorhiwidget.cpp
)

set(nanovg_base ../../nanovg)
set(nanovg_target nanorhiwidget)
include(${nanovg_base}/nanovg.cmakeinc)

target_link_libraries(nanorhiwidget PUBLIC
    Qt::Core
    Qt::GuiPrivate
    Qt::Widgets
)

qt6_add_shaders(nanorhiwidget "shaders"
    PREFIX
        "/shaders"
    FILES
        color.vert
        color.frag
)

qt6_add_resources(nanorhiwidget "resources"
    PREFIX
        "/"
    BASE
        "../shared"
    FILES
        "../shared/fonts/RobotoMono-Medium.ttf"
)

cmake_minimum_required(VERSION 3.20)
project(nanoitem LANGUAGES C CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 COMPONENTS Core)
find_package(Qt6 COMPONENTS Gui)
find_package(Qt6 COMPONENTS Quick)
find_package(Qt6 COMPONENTS ShaderTools)

qt_add_executable(nanoitem
    main.cpp
    testitem.cpp testitem.h
)

set(nanovg_base ../../nanovg)
set(nanovg_target nanoitem)
include(${nanovg_base}/nanovg.cmakeinc)

target_link_libraries(nanoitem PUBLIC
    Qt::Core
    Qt::GuiPrivate
    Qt::QuickPrivate
)

qt_add_qml_module(nanoitem
    URI NanoItemExample
    VERSION 1.0
    QML_FILES
        main.qml
    RESOURCE_PREFIX
        /
    NO_RESOURCE_TARGET_PATH
)

TARGET_COMPILE_DEFINITIONS(nanoitem
  PRIVATE $<$<OR:$<CONFIG:Debug>,$<CONFIG:RelWithDebInfo>>:QT_QML_DEBUG>)

qt_add_resources(nanoitem "resources"
    PREFIX
        "/"
    BASE
        "../shared"
    FILES
        "../shared/fonts/RobotoMono-Medium.ttf"
)

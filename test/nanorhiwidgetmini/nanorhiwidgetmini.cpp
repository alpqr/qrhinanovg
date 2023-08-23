// Copyright (C) 2023 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include <QApplication>
#include <QRhiWidget>
#include <rhi/qrhi.h>
#include "nanovg_rhi.h"

class Widget : public QRhiWidget
{
public:
    void initialize(QRhiCommandBuffer *cb) override;
    void render(QRhiCommandBuffer *cb) override;

private:
    NanoVG vg;
    int rotation = 0;
};

void Widget::initialize(QRhiCommandBuffer *)
{
    if (!vg.isValid())
        vg.create(rhi(), NVG_ANTIALIAS | NVG_STENCIL_STROKES);
}

void Widget::render(QRhiCommandBuffer *cb)
{
    vg.begin(cb, renderTarget(), devicePixelRatio());
    nvgTranslate(vg.ctx, 100, 100);
    nvgRotate(vg.ctx, qDegreesToRadians(rotation++));
    nvgTranslate(vg.ctx, -100, -100);
    nvgBeginPath(vg.ctx);
    nvgRect(vg.ctx, 10, 10, 200, 200);
    nvgFillColor(vg.ctx, nvgRGBA(255, 0, 0, 255));
    nvgFill(vg.ctx);
    vg.end();

    cb->beginPass(renderTarget(), QColor(0, 128, 0), { 1.0f, 0 });
    vg.render();
    cb->endPass();

    update();
}

int main(int argc, char **argv)
{
    QApplication app(argc, argv);
    Widget w;
    // Rendering backend defaults are D3D11 on Windows, Metal on Apple, OpenGL otherwise.
    // Call e.g. w.setApi(QRhiWidget::Api::Vulkan) to request something else.
    w.resize(1280, 720);
    w.show();
    return app.exec();
}

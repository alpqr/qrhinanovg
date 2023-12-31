Qt (QRhi) backend for https://github.com/memononen/nanovg

3D API independent, should work with anything QRhi supports (D3D11, D3D12,
Vulkan, Metal, OpenGL), on all platforms QRhi and Qt works.

However, nanovg_rhi.cpp and .h require Qt 6.6 (where QRhi is finally
semi-public, i.e. including <rhi/qrhi.h> works), whereas some of the examples
rely on work-in-progress Qt 6.7 APIs (QRhiWidget, QQuickRhiItem).

Won't build with older Qt versions, although the integration and the
QWindow-based example could be backported.

For a higher level, Qt-style API that completely wraps NanoVG, check out
https://github.com/QUItCoding/qnanopainter which currently contains the a
slightly modified version of the code here as an experimental option.

In contrast, this repo and the examples here focus on directly working with the
NanoVG API (no wrappers) while rendering to any QRhi-based render target.

This NanoVG adaptation is somewhat different from others since a plain nvgBeginFrame/EndFrame is not suitable.

Usage:

```
NanoVG vg;

vg.create(rhi, NVG_ANTIALIAS | NVG_STENCIL_STROKES);

Then within a frame (after rhi->beginFrame()) but outside a render pass:

vg.begin(cb, rt, offset, dpr); // corresponds to nvgBeginFrame()
nvgBeginPath(m_vg.ctx);
nvgRect(m_vg.ctx, 10, 10, 100, 100);
nvgFillColor(m_vg.ctx, nvgRGBA(220, 0, 0, 255));
nvgFill(m_vg.ctx);
...
m_vg.end(); // partially corresponds to nvgEndFrame()

Then once a renderpass is being recorded:

cb->beginPass(rt, QColor(0, 128, 0), { 1.0f, 0 });
...
m_vg.render(); // nvgEndFrame() phase #2
cb->endPass();
```

There is no stb_image and so nvgCreateImage[Mem], use this instead:

```
QImage img;
img.load(QLatin1String(":/image.png"));
imageId = nvgCreateImageRGBA(vg.ctx, img.width(), img.height(), 0, img.constBits());
```

![Screenshot](nanoitem_screenshot.png)
(screenshot of the nanoitem example)

Example apps:
- nanowindow: Rendering in a plain QWindow (no QWidgets, no QML) (requires Qt 6.6)
- nanorhwidget: Rendering in a QRhiWidget (requires Qt 6.7, coming 2024)
- nanoitem: Rendering in a QQuickRhiItem in a Qt Quick scene (requires Qt 6.7, could be ported to 6.6 by not using QQuickRhiItem)

Below is a minimal, complete QRhiWidget-based application with a rotating red rectangle:

```
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
    vg.begin(cb, renderTarget(), QPointF(0, 0), devicePixelRatio());
    nvgTranslate(vg.ctx, 100, 100);
    nvgRotate(vg.ctx, qDegreesToRadians(rotation++));
    nvgTranslate(vg.ctx, -100, -100);
    nvgBeginPath(vg.ctx);
    nvgRect(vg.ctx, 0, 0, 200, 200);
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
```

Qt (QRhi) backend for https://github.com/memononen/nanovg

3D API independent (should work with anything QRhi supports)

The adaptation is somewhat different from others since a plain BeginFrame/EndFrame is not suitable.
Instead EndFrame() is split to a prepare-render phase.

```
NanoVG vg;

vg.create(rhi, NVG_ANTIALIAS | NVG_STENCIL_STROKES);

Then within a frame (after rhi->beginFrame()) but outside a render pass:

vg.begin(cb, rt, dpr);
nvgBeginPath(m_vg.ctx);
nvgRect(m_vg.ctx, 10, 10, 100, 100);
nvgFillColor(m_vg.ctx, nvgRGBA(220, 0, 0, 255));
nvgFill(m_vg.ctx);
...
m_vg.end();

Then once a renderpass is being recorded:

cb->beginPass(rt, QColor(0, 128, 0), { 1.0f, 0 });
...
m_vg.render();
cb->endPass();
```

There is no stb_image and so nvgCreateImage[Mem], use this instead:

```
QImage img;
img.load(QLatin1String(":/image.png"));
imageId = nvgCreateImageRGBA(vg.ctx, img.width(), img.height(), 0, img.constBits());
```

Example apps:
- nanowindow: Rendering in a plain QWindow (no QWidgets, no QML)
- nanorhwidget: Rendering in a QRhiWidget (requires Qt 6.7, coming 2024)
- nanoitem: Rendering in a QQuickRhiItem in a Qt Quick scene (requires Qt 6.7, could be ported to 6.6 by not using QQuickRhiItem)

A bunch of NanoVG features are probably not exercised and so tested; should port the full upstream demo some day.

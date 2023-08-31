// Copyright (C) 2023 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include "testitem.h"
#include <QQuickWindow>
#include <QFile>
#include <rhi/qrhi.h>

#include "../shared/demo.h"

TestItem::TestItem(QQuickItem *parent)
    : QQuickRhiItem(parent)
{
    setAcceptedMouseButtons(Qt::LeftButton);
    setAcceptHoverEvents(true);
}

QQuickRhiItemRenderer *TestItem::createRenderer()
{
    return new TestItemRenderer;
}

void TestItem::mousePressEvent(QMouseEvent *e)
{
    e->accept();
}

void TestItem::mouseMoveEvent(QMouseEvent *e)
{
    e->accept();
    m_mousePos = e->position();
    update();
}

void TestItem::hoverMoveEvent(QHoverEvent *e)
{
    e->accept();
    m_mousePos = e->position();
    update();
}

// All of TestItemRenderer lives and runs on the render thread (if there is
// one). The destructor / member variables runs / are destroyed on the render
// thread as well, which is perfect for us (since m_vg can just be a member and
// don't even need a destructor for TestItemRenderer).

TestItemRenderer::~TestItemRenderer()
{
    reset();
}

void TestItemRenderer::reset()
{
    if (m_vg.isValid()) {
        if (m_imageId)
            nvgDeleteImage(m_vg.ctx, m_imageId);
        freeDemoData(m_vg.ctx, &demoData);
        m_vg.destroy();
    }
}

void TestItemRenderer::initialize(QRhiCommandBuffer *)
{
    if (rhi() != m_rhi) {
        m_rhi = rhi();
        reset();
    }

    if (renderTarget() != m_rt) {
        m_rt = renderTarget();
        // in case the QRhiRenderPassDescriptor is incompatible with the new rt
        reset();
    }

    if (!m_vg.isValid()) {
        m_vg.create(m_rhi, NVG_ANTIALIAS | NVG_STENCIL_STROKES);

        createFont(m_vg.ctx, "font", ":/fonts/RobotoMono-Medium.ttf");
        m_imageId = createImage(m_vg.ctx, ":/qtlogo.png");

        loadDemoData(m_vg.ctx, &demoData);
    }
}

void TestItemRenderer::synchronize(QQuickRhiItem *rhiItem)
{
    // render thread (if there is one), with main thread blocked, hence it is safe to read/write data

    TestItem *item = static_cast<TestItem *>(rhiItem);
    m_dpr = item->window()->effectiveDevicePixelRatio();
    demoData.mousePos = item->m_mousePos.toPoint();
    demoData.t = item->m_t;
    demoData.blowup = item->m_blowUp;
}

void TestItemRenderer::render(QRhiCommandBuffer *cb)
{
    m_vg.begin(cb, m_rt, {}, m_dpr);

    nvgBeginPath(m_vg.ctx);
    int imageWidth, imageHeight;
    nvgImageSize(m_vg.ctx, m_imageId, &imageWidth, &imageHeight);
    nvgRect(m_vg.ctx, 10, 10, imageWidth, imageHeight);
    NVGpaint imagePaint = nvgImagePattern(m_vg.ctx, 10, 10, imageWidth, imageHeight, 0, m_imageId, 1);
    nvgFillPaint(m_vg.ctx, imagePaint);
    nvgFill(m_vg.ctx);

    nvgFontFace(m_vg.ctx, "font");
    nvgFontSize(m_vg.ctx, 36.0f);
    nvgFillColor(m_vg.ctx, nvgRGBA(220, 0, 220, 255));
    nvgText(m_vg.ctx, 500, 50, "hello world", nullptr);

    const int w = m_rt->pixelSize().width() / m_dpr;
    const int h = m_rt->pixelSize().height() / m_dpr;
    renderDemo(m_vg.ctx, demoData.mousePos.x(), demoData.mousePos.y(), w, h, demoData.t, demoData.blowup, &demoData);

    m_vg.end();

    cb->beginPass(m_rt, QColor(0, 128, 0), { 1.0f, 0 });
    m_vg.render();
    cb->endPass();
}

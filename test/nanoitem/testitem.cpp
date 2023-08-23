// Copyright (C) 2023 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include "testitem.h"
#include <QQuickWindow>
#include <QFile>
#include <rhi/qrhi.h>

QT_BEGIN_NAMESPACE

TestItem::TestItem(QQuickItem *parent)
    : QQuickRhiItem(parent)
{
    setAcceptedMouseButtons(Qt::LeftButton);
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
    m_lookPos = e->scenePosition();
    update();
}

// All of TestItemRenderer lives and runs on the render thread (if there is
// one). The destructor / member variables runs / are destroyed on the render
// thread as well, which is perfect for us (since m_vg can just be a member and
// don't even need a destructor for TestItemRenderer).

static QByteArray getFile(const QString &name)
{
    QFile f(name);
    return f.open(QIODevice::ReadOnly) ? f.readAll() : QByteArray();
}

void TestItemRenderer::initialize(QRhiCommandBuffer *cb)
{
    if (rhi() != m_rhi) {
        m_rhi = rhi();
        m_vg.destroy();
    }
    if (renderTarget() != m_rt) {
        m_rt = renderTarget();
        // in case the QRhiRenderPassDescriptor is incompatible with the new rt
        m_vg.destroy();
    }
    if (!m_vg.isValid()) {
        m_vg.create(m_rhi, NVG_ANTIALIAS | NVG_STENCIL_STROKES);
        QByteArray font = getFile(QLatin1String(":/fonts/RobotoMono-Medium.ttf"));
        unsigned char *fontData = (unsigned char *) malloc(font.size());
        memcpy(fontData, font.constData(), font.size());
        nvgCreateFontMem(m_vg.ctx, "font", fontData, font.size(), 1);
    }
}

void TestItemRenderer::synchronize(QQuickRhiItem *rhiItem)
{
    // render thread (if there is one), with main thread blocked, hence it is safe to read/write data

    TestItem *item = static_cast<TestItem *>(rhiItem);
    m_dpr = item->window()->effectiveDevicePixelRatio();
    m_lookPos = item->m_lookPos;
}

static void drawEyes(NVGcontext* vg, float x, float y, float w, float h, float mx, float my, float t);

void TestItemRenderer::render(QRhiCommandBuffer *cb)
{
    m_vg.begin(cb, m_rt, m_dpr);

    nvgBeginPath(m_vg.ctx);
    nvgRect(m_vg.ctx, 10, 10, 100, 100);
    nvgFillColor(m_vg.ctx, nvgRGBA(220, 0, 0, 255));
    nvgFill(m_vg.ctx);

    nvgFontFace(m_vg.ctx, "font");
    nvgFontSize(m_vg.ctx, 36.0f);
    nvgFillColor(m_vg.ctx, nvgRGBA(220, 0, 220, 255));
    nvgText(m_vg.ctx, 10, 300, "hello world", nullptr);

    drawEyes(m_vg.ctx, 200, 10, 120, 120, m_lookPos.x(), m_lookPos.y(), 1);

    m_vg.end();

    cb->beginPass(m_rt, QColor(0, 128, 0), { 1.0f, 0 });
    m_vg.render();
    cb->endPass();
}

static void drawEyes(NVGcontext* vg, float x, float y, float w, float h, float mx, float my, float t)
{
    NVGpaint gloss, bg;
    float ex = w *0.23f;
    float ey = h * 0.5f;
    float lx = x + ex;
    float ly = y + ey;
    float rx = x + w - ex;
    float ry = y + ey;
    float dx,dy,d;
    float br = (ex < ey ? ex : ey) * 0.5f;
    float blink = 1 - pow(sinf(t*0.5f),200)*0.8f;

    bg = nvgLinearGradient(vg, x,y+h*0.5f,x+w*0.1f,y+h, nvgRGBA(0,0,0,32), nvgRGBA(0,0,0,16));
    nvgBeginPath(vg);
    nvgEllipse(vg, lx+3.0f,ly+16.0f, ex,ey);
    nvgEllipse(vg, rx+3.0f,ry+16.0f, ex,ey);
    nvgFillPaint(vg, bg);
    nvgFill(vg);

    bg = nvgLinearGradient(vg, x,y+h*0.25f,x+w*0.1f,y+h, nvgRGBA(220,220,220,255), nvgRGBA(128,128,128,255));
    nvgBeginPath(vg);
    nvgEllipse(vg, lx,ly, ex,ey);
    nvgEllipse(vg, rx,ry, ex,ey);
    nvgFillPaint(vg, bg);
    nvgFill(vg);

    dx = (mx - rx) / (ex * 10);
    dy = (my - ry) / (ey * 10);
    d = sqrtf(dx*dx+dy*dy);
    if (d > 1.0f) {
        dx /= d; dy /= d;
    }
    dx *= ex*0.4f;
    dy *= ey*0.5f;
    nvgBeginPath(vg);
    nvgEllipse(vg, lx+dx,ly+dy+ey*0.25f*(1-blink), br,br*blink);
    nvgFillColor(vg, nvgRGBA(32,32,32,255));
    nvgFill(vg);

    dx = (mx - rx) / (ex * 10);
    dy = (my - ry) / (ey * 10);
    d = sqrtf(dx*dx+dy*dy);
    if (d > 1.0f) {
        dx /= d; dy /= d;
    }
    dx *= ex*0.4f;
    dy *= ey*0.5f;
    nvgBeginPath(vg);
    nvgEllipse(vg, rx+dx,ry+dy+ey*0.25f*(1-blink), br,br*blink);
    nvgFillColor(vg, nvgRGBA(32,32,32,255));
    nvgFill(vg);

    gloss = nvgRadialGradient(vg, lx-ex*0.25f,ly-ey*0.5f, ex*0.1f,ex*0.75f, nvgRGBA(255,255,255,128), nvgRGBA(255,255,255,0));
    nvgBeginPath(vg);
    nvgEllipse(vg, lx,ly, ex,ey);
    nvgFillPaint(vg, gloss);
    nvgFill(vg);

    gloss = nvgRadialGradient(vg, rx-ex*0.25f,ry-ey*0.5f, ex*0.1f,ex*0.75f, nvgRGBA(255,255,255,128), nvgRGBA(255,255,255,0));
    nvgBeginPath(vg);
    nvgEllipse(vg, rx,ry, ex,ey);
    nvgFillPaint(vg, gloss);
    nvgFill(vg);
}

QT_END_NAMESPACE

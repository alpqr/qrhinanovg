// Copyright (C) 2022 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include <QGuiApplication>
#include <QQuickView>
#include <QFile>
#include "testitem.h"

static QByteArray getFile(const QString &name)
{
    QFile f(name);
    return f.open(QIODevice::ReadOnly) ? f.readAll() : QByteArray();
}

TestPainter::~TestPainter()
{
}

void TestPainter::sync(NanoItem *)
{
}

void TestPainter::prepare(QRhi *rhi, QRhiCommandBuffer *cb, QRhiRenderTarget *rt, float opacity, const QMatrix4x4 &mvp)
{
    if (!m_vg.isValid()) {
        m_vg.create(rhi, NVG_ANTIALIAS | NVG_STENCIL_STROKES);
        QByteArray font = getFile(QLatin1String(":/fonts/RobotoMono-Medium.ttf"));
        unsigned char *fontData = (unsigned char *) malloc(font.size());
        memcpy(fontData, font.constData(), font.size());
        nvgCreateFontMem(m_vg.ctx, "font", fontData, font.size(), 1);
    }

    // opacity and mvp are not used atm

    m_vg.begin(cb, rt);

    nvgBeginPath(m_vg.ctx);
    nvgRect(m_vg.ctx, 10, 10, 100, 100);
    nvgFillColor(m_vg.ctx, nvgRGBA(220, 0, 0, 255));
    nvgFill(m_vg.ctx);

    nvgFontFace(m_vg.ctx, "font");
    nvgFontSize(m_vg.ctx, 36.0f);
    nvgFillColor(m_vg.ctx, nvgRGBA(220, 0, 220, 255));
    nvgText(m_vg.ctx, 10, 300, "hello world", nullptr);

    m_vg.end();

}

void TestPainter::render()
{
    m_vg.render();
}

int main(int argc, char *argv[])
{
    QGuiApplication app(argc, argv);

    QQuickView view;
    view.setColor(Qt::black);
    view.setResizeMode(QQuickView::SizeRootObjectToView);
    view.resize(1280, 720);
    view.setSource(QUrl("qrc:/main.qml"));
    view.show();

    return app.exec();
}

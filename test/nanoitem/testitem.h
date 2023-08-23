// Copyright (C) 2022 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#ifndef TESTITEM_H
#define TESTITEM_H

#include <QQuickRhiItem>
#include "nanovg_rhi.h"

class TestItemRenderer : public QQuickRhiItemRenderer
{
public:
    void initialize(QRhiCommandBuffer *cb) override;
    void synchronize(QQuickRhiItem *item) override;
    void render(QRhiCommandBuffer *cb) override;

private:
    QRhi *m_rhi = nullptr;
    QRhiRenderTarget *m_rt = nullptr;
    float m_dpr = 1.0f;

    NanoVG m_vg;

    QPointF m_lookPos;
};

class TestItem : public QQuickRhiItem
{
    Q_OBJECT
    QML_NAMED_ELEMENT(TestNanoItem)

public:
    TestItem(QQuickItem *parent = nullptr);
    QQuickRhiItemRenderer *createRenderer() override;
    void mousePressEvent(QMouseEvent *e) override;
    void mouseMoveEvent(QMouseEvent *e) override;

    QPointF m_lookPos;
};

#endif

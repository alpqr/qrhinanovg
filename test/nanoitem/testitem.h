// Copyright (C) 2022 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#ifndef TESTITEM_H
#define TESTITEM_H

#include <QQuickRhiItem>
#include "nanovg_rhi.h"

class TestItemRenderer : public QQuickRhiItemRenderer
{
public:
    ~TestItemRenderer();
    void initialize(QRhiCommandBuffer *cb) override;
    void synchronize(QQuickRhiItem *item) override;
    void render(QRhiCommandBuffer *cb) override;

private:
    void reset();

    QRhi *m_rhi = nullptr;
    QRhiRenderTarget *m_rt = nullptr;
    float m_dpr = 1.0f;

    NanoVG m_vg;

    int m_imageId = 0;
};

class TestItem : public QQuickRhiItem
{
    Q_OBJECT

    QML_NAMED_ELEMENT(TestNanoItem)

    Q_PROPERTY(float t READ t WRITE setT NOTIFY tChanged)
    Q_PROPERTY(bool blowUp READ blowUp WRITE setBlowUp NOTIFY blowUpChanged)

public:
    TestItem(QQuickItem *parent = nullptr);
    QQuickRhiItemRenderer *createRenderer() override;
    void mousePressEvent(QMouseEvent *e) override;
    void mouseMoveEvent(QMouseEvent *e) override;
    void hoverMoveEvent(QHoverEvent *) override;

    float t() const { return m_t; }
    void setT(float value) { if (m_t != value) { m_t = value; emit tChanged(); update(); } }

    bool blowUp() const { return m_blowUp; }
    void setBlowUp(bool value) { if (m_blowUp != value) { m_blowUp = value; emit blowUpChanged(); update(); } }

    float m_t = 0.0f;
    bool m_blowUp = false;
    QPointF m_mousePos;

signals:
    void tChanged();
    void blowUpChanged();
};

#endif

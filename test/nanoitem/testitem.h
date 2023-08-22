// Copyright (C) 2022 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#ifndef TESTITEM_H
#define TESTITEM_H

#include "nanoitem.h"
#include "nanovg_rhi.h"

class TestPainter : public NanoItemRenderer
{
public:
    ~TestPainter();
    void sync(NanoItem *item) override;
    void prepare(QRhi *rhi, QRhiCommandBuffer *cb, QRhiRenderTarget *rt, float opacity, const QMatrix4x4 &mvp) override;
    void render() override;

private:
    NanoVG m_vg;
};

class TestNanoItem : public NanoItem
{
    Q_OBJECT
    QML_NAMED_ELEMENT(TestNanoItem)

public:
    NanoItemRenderer *createRenderer() override { return new TestPainter; }

private:
    NanoVG m_vg;
};

#endif

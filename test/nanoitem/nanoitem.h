// Copyright (C) 2022 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#ifndef NANOITEM_H
#define NANOITEM_H

#include <QtQuick/qquickitem.h>

QT_BEGIN_NAMESPACE

class QRhi;
class QRhiCommandBuffer;
class QRhiRenderTarget;
class NanoItem;

class NanoItemRenderer
{
public:
    virtual ~NanoItemRenderer() { }
    virtual void sync(NanoItem *item) = 0;
    virtual void prepare(QRhi *rhi, QRhiCommandBuffer *cb, QRhiRenderTarget *rt, float opacity, const QMatrix4x4 &mvp) = 0;
    virtual void render() = 0;
};

class NanoItem : public QQuickItem
{
    Q_OBJECT

public:
    NanoItem(QQuickItem *parent = nullptr);
    ~NanoItem();

    virtual NanoItemRenderer *createRenderer() = 0;

private:
    QSGNode *updatePaintNode(QSGNode *, UpdatePaintNodeData *) override;
};

QT_END_NAMESPACE

#endif

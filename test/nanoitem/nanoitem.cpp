// Copyright (C) 2022 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include "nanoitem.h"
#include <QtGui/qguiapplication.h>
#include <QtQuick/qquickwindow.h>
#include <QtQuick/private/qsgrendernode_p.h>

QT_BEGIN_NAMESPACE

class NanoItem;

struct NanoNode : public QSGRenderNode
{
    NanoNode(QQuickWindow *window, NanoItem *item);
    ~NanoNode();

    void prepare() override;
    void render(const RenderState *state) override;
    void releaseResources() override;
    StateFlags changedStates() const override;
    RenderingFlags flags() const override;

    QQuickWindow *window;
    NanoItem *item;
    NanoItemRenderer *renderer = nullptr;
};

NanoNode::NanoNode(QQuickWindow *window, NanoItem *item)
    : window(window),
      item(item)
{
}

NanoNode::~NanoNode()
{
    delete renderer;
}

void NanoNode::releaseResources()
{
    delete renderer;
    renderer = nullptr;
}

void NanoNode::prepare()
{
    if (!renderer)
        return;

    QRhi *rhi = window->rhi();
    if (!rhi) {
        qWarning("QRhiImguiNode: No QRhi found for window %p", window);
        return;
    }

    const QMatrix4x4 mvp = *projectionMatrix() * *matrix();

    renderer->prepare(rhi, commandBuffer(), renderTarget(), inheritedOpacity(), mvp);
}

void NanoNode::render(const RenderState *)
{
    if (!renderer)
        return;

    renderer->render();
}

QSGRenderNode::StateFlags NanoNode::changedStates() const
{
    return DepthState | ScissorState | ColorState | BlendState | CullState | ViewportState;
}

QSGRenderNode::RenderingFlags NanoNode::flags() const
{
    return NoExternalRendering | DepthAwareRendering;
}

NanoItem::NanoItem(QQuickItem *parent)
    : QQuickItem(parent)
{
    setFlag(ItemHasContents, true);
}

NanoItem::~NanoItem()
{
}

QSGNode *NanoItem::updatePaintNode(QSGNode *node, QQuickItem::UpdatePaintNodeData *)
{
    // render thread, with main thread blocked

    if (size().isEmpty()) {
        delete node;
        return nullptr;
    }

    NanoNode *n = static_cast<NanoNode *>(node);
    if (!n)
        n = new NanoNode(window(), this);

    if (!n->renderer)
        n->renderer = createRenderer();

    n->renderer->sync(this);

    n->markDirty(QSGNode::DirtyMaterial);
    return n;
}

QT_END_NAMESPACE

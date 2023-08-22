// Copyright (C) 2023 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include <QApplication>
#include <QCommandLineParser>
#include <QRhiWidget>
#include <QVBoxLayout>
#include <QFile>
#include <rhi/qrhi.h>
#include "nanovg_rhi.h"

static float vertexData[] = {
     0.0f,   0.5f,   1.0f, 0.0f, 0.0f,
    -0.5f,  -0.5f,   0.0f, 1.0f, 0.0f,
     0.5f,  -0.5f,   0.0f, 0.0f, 1.0f,
};

static QShader getShader(const QString &name)
{
    QFile f(name);
    return f.open(QIODevice::ReadOnly) ? QShader::fromSerialized(f.readAll()) : QShader();
}

static QByteArray getFile(const QString &name)
{
    QFile f(name);
    return f.open(QIODevice::ReadOnly) ? f.readAll() : QByteArray();
}

class Widget : public QRhiWidget
{
public:
    Widget(QWidget *parent = nullptr) : QRhiWidget(parent) { }
    ~Widget();

    void initialize(QRhiCommandBuffer *cb) override;
    void render(QRhiCommandBuffer *cb) override;

private:
    QRhi *m_rhi = nullptr;
    QMatrix4x4 m_proj;
    std::unique_ptr<QRhiBuffer> m_vbuf;
    std::unique_ptr<QRhiBuffer> m_ubuf;
    std::unique_ptr<QRhiShaderResourceBindings> m_srb;
    std::unique_ptr<QRhiGraphicsPipeline> m_ps;

    float m_rotation = 0;
    float m_opacity = 1;
    int m_opacityDir = -1;

    NanoVG m_vg;
    int m_imageId = 0;
};

Widget::~Widget()
{
    if (m_imageId)
        nvgDeleteImage(m_vg.ctx, m_imageId);
}

void Widget::initialize(QRhiCommandBuffer *cb)
{
    if (m_rhi != rhi()) {
        m_ps.reset();
        m_rhi = rhi();
    }
    if (!m_ps) {
        m_vbuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::VertexBuffer, sizeof(vertexData)));
        m_vbuf->create();

        m_ubuf.reset(m_rhi->newBuffer(QRhiBuffer::Dynamic, QRhiBuffer::UniformBuffer, 68));
        m_ubuf->create();

        m_srb.reset(m_rhi->newShaderResourceBindings());
        m_srb->setBindings({
                QRhiShaderResourceBinding::uniformBuffer(0, QRhiShaderResourceBinding::VertexStage | QRhiShaderResourceBinding::FragmentStage,
                                                        m_ubuf.get())
        });
        m_srb->create();

        m_ps.reset(m_rhi->newGraphicsPipeline());

        QRhiGraphicsPipeline::TargetBlend premulAlphaBlend;
        premulAlphaBlend.enable = true;
        m_ps->setTargetBlends({ premulAlphaBlend });

        m_ps->setShaderStages({
            { QRhiShaderStage::Vertex, getShader(QLatin1String(":/shaders/color.vert.qsb")) },
            { QRhiShaderStage::Fragment, getShader(QLatin1String(":/shaders/color.frag.qsb")) }
        });

        QRhiVertexInputLayout inputLayout;
        inputLayout.setBindings({
            { 5 * sizeof(float) }
        });
        inputLayout.setAttributes({
            { 0, 0, QRhiVertexInputAttribute::Float2, 0 },
            { 0, 1, QRhiVertexInputAttribute::Float3, 2 * sizeof(float) }
        });

        m_ps->setVertexInputLayout(inputLayout);
        m_ps->setShaderResourceBindings(m_srb.get());
        m_ps->setRenderPassDescriptor(renderTarget()->renderPassDescriptor());

        m_ps->create();

        QRhiResourceUpdateBatch *resourceUpdates = m_rhi->nextResourceUpdateBatch();
        resourceUpdates->uploadStaticBuffer(m_vbuf.get(), vertexData);
        cb->resourceUpdate(resourceUpdates);

        m_vg.create(m_rhi, NVG_ANTIALIAS | NVG_STENCIL_STROKES);
        QByteArray font = getFile(QLatin1String(":/fonts/RobotoMono-Medium.ttf"));
        unsigned char *fontData = (unsigned char *) malloc(font.size());
        memcpy(fontData, font.constData(), font.size());
        nvgCreateFontMem(m_vg.ctx, "font", fontData, font.size(), 1);

        QImage img;
        img.load(QLatin1String(":/qtlogo.png"));
        m_imageId = nvgCreateImageRGBA(m_vg.ctx, img.width(), img.height(), 0, img.constBits());
    }

    const QSize outputSize = renderTarget()->pixelSize();
    m_proj = m_rhi->clipSpaceCorrMatrix();
    m_proj.perspective(45.0f, outputSize.width() / (float) outputSize.height(), 0.01f, 1000.0f);
    m_proj.translate(0, 0, -4);
}

void Widget::render(QRhiCommandBuffer *cb)
{
    QRhiResourceUpdateBatch *u = m_rhi->nextResourceUpdateBatch();
    m_rotation += 1.0f;
    QMatrix4x4 mvp = m_proj;
    mvp.rotate(m_rotation, 0, 1, 0);
    u->updateDynamicBuffer(m_ubuf.get(), 0, 64, mvp.constData());
    m_opacity += m_opacityDir * 0.005f;
    if (m_opacity < 0.0f || m_opacity > 1.0f) {
        m_opacityDir *= -1;
        m_opacity = qBound(0.0f, m_opacity, 1.0f);
    }
    u->updateDynamicBuffer(m_ubuf.get(), 64, 4, &m_opacity);

    m_vg.begin(cb, renderTarget());

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
    nvgText(m_vg.ctx, 10, 300, "hello world", nullptr);

    m_vg.end();

    const QSize outputSizeInPixels = renderTarget()->pixelSize();
    cb->beginPass(renderTarget(), QColor::fromRgbF(0.4f, 0.7f, 0.0f, 1.0f), { 1.0f, 0 }, u);

    cb->setGraphicsPipeline(m_ps.get());
    cb->setViewport({ 0, 0, float(outputSizeInPixels.width()), float(outputSizeInPixels.height()) });
    cb->setShaderResources();

    const QRhiCommandBuffer::VertexInput vbufBinding(m_vbuf.get(), 0);
    cb->setVertexInput(0, 1, &vbufBinding);
    cb->draw(3);

    m_vg.render();

    cb->endPass();

    update();
}

struct ScrollAreaKeyFilter : public QObject
{
    ScrollAreaKeyFilter(QObject *target) : m_target(target) { }
    bool eventFilter(QObject *obj, QEvent *e) override {
        switch (e->type()) {
        case QEvent::KeyPress:
        case QEvent::KeyRelease:
            QCoreApplication::sendEvent(m_target, e);
            return true;
        default:
            break;
        }
        return QObject::eventFilter(obj, e);
    }
    QObject *m_target;
};

int main(int argc, char **argv)
{
    QApplication app(argc, argv);

    QRhiWidget::Api graphicsApi;
#if defined(Q_OS_WIN)
    graphicsApi = QRhiWidget::Api::D3D11;
#elif defined(Q_OS_MACOS) || defined(Q_OS_IOS)
    graphicsApi = QRhiWidget::Api::Metal;
#elif QT_CONFIG(vulkan)
    graphicsApi = QRhiWidget::Api::Vulkan;
#else
    graphicsApi = QRhiWidget::Api::OpenGL;
#endif

    QCommandLineParser cmdLineParser;
    cmdLineParser.addHelpOption();
    QCommandLineOption nullOption({ "n", "null" }, QLatin1String("Null"));
    cmdLineParser.addOption(nullOption);
    QCommandLineOption glOption({ "g", "opengl" }, QLatin1String("OpenGL"));
    cmdLineParser.addOption(glOption);
    QCommandLineOption vkOption({ "v", "vulkan" }, QLatin1String("Vulkan"));
    cmdLineParser.addOption(vkOption);
    QCommandLineOption d3d11Option({ "d", "d3d11" }, QLatin1String("Direct3D 11"));
    cmdLineParser.addOption(d3d11Option);
    QCommandLineOption d3d12Option({ "D", "d3d12" }, QLatin1String("Direct3D 12"));
    cmdLineParser.addOption(d3d12Option);
    QCommandLineOption mtlOption({ "m", "metal" }, QLatin1String("Metal"));
    cmdLineParser.addOption(mtlOption);

    cmdLineParser.process(app);
    if (cmdLineParser.isSet(nullOption))
        graphicsApi = QRhiWidget::Api::Null;
    if (cmdLineParser.isSet(glOption))
        graphicsApi = QRhiWidget::Api::OpenGL;
    if (cmdLineParser.isSet(vkOption))
        graphicsApi = QRhiWidget::Api::Vulkan;
    if (cmdLineParser.isSet(d3d11Option))
        graphicsApi = QRhiWidget::Api::D3D11;
    if (cmdLineParser.isSet(d3d12Option))
        graphicsApi = QRhiWidget::Api::D3D12;
    if (cmdLineParser.isSet(mtlOption))
        graphicsApi = QRhiWidget::Api::Metal;

    qDebug() << "Selected graphics API is" << graphicsApi;
    qDebug("This is a multi-api example, use command line arguments to override:\n%s", qPrintable(cmdLineParser.helpText()));

    Widget *rhiWidget = new Widget;
    rhiWidget->setApi(graphicsApi);
    rhiWidget->setDebugLayer(true);

    QVBoxLayout *layout = new QVBoxLayout;
    layout->addWidget(rhiWidget);

    QWidget topLevel;
    topLevel.setLayout(layout);
    topLevel.resize(1280, 720);

    topLevel.show();
    return app.exec();
}

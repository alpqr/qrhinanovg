// Copyright (C) 2023 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include <QGuiApplication>
#include <QCommandLineParser>
#include <QWindow>
#include <QPlatformSurfaceEvent>
#include <QOffscreenSurface>
#include <QFile>
#include <rhi/qrhi.h>

#include "nanovg_rhi.h"

static QByteArray getFile(const QString &name)
{
    QFile f(name);
    return f.open(QIODevice::ReadOnly) ? f.readAll() : QByteArray();
}

NanoVG vg;

// called once
void initTest(QRhi *rhi)
{
    vg.create(rhi, NVG_ANTIALIAS | NVG_STENCIL_STROKES);

    QByteArray font = getFile(QLatin1String(":/fonts/RobotoMono-Medium.ttf"));
    unsigned char *fontData = (unsigned char *) malloc(font.size());
    memcpy(fontData, font.constData(), font.size());

    nvgCreateFontMem(vg.ctx, "font", fontData, font.size(), 1);
}

void cleanupTest()
{
    vg.destroy();
}

void drawEyes(NVGcontext* vg, float x, float y, float w, float h, float mx, float my, float t)
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

void drawGraph(NVGcontext* vg, float x, float y, float w, float h, float t)
{
    NVGpaint bg;
    float samples[6];
    float sx[6], sy[6];
    float dx = w/5.0f;
    int i;

    samples[0] = (1+sinf(t*1.2345f+cosf(t*0.33457f)*0.44f))*0.5f;
    samples[1] = (1+sinf(t*0.68363f+cosf(t*1.3f)*1.55f))*0.5f;
    samples[2] = (1+sinf(t*1.1642f+cosf(t*0.33457)*1.24f))*0.5f;
    samples[3] = (1+sinf(t*0.56345f+cosf(t*1.63f)*0.14f))*0.5f;
    samples[4] = (1+sinf(t*1.6245f+cosf(t*0.254f)*0.3f))*0.5f;
    samples[5] = (1+sinf(t*0.345f+cosf(t*0.03f)*0.6f))*0.5f;

    for (i = 0; i < 6; i++) {
        sx[i] = x+i*dx;
        sy[i] = y+h*samples[i]*0.8f;
    }

    // Graph background
    bg = nvgLinearGradient(vg, x,y,x,y+h, nvgRGBA(0,160,192,0), nvgRGBA(0,160,192,64));
    nvgBeginPath(vg);
    nvgMoveTo(vg, sx[0], sy[0]);
    for (i = 1; i < 6; i++)
        nvgBezierTo(vg, sx[i-1]+dx*0.5f,sy[i-1], sx[i]-dx*0.5f,sy[i], sx[i],sy[i]);
    nvgLineTo(vg, x+w, y+h);
    nvgLineTo(vg, x, y+h);
    nvgFillPaint(vg, bg);
    nvgFill(vg);

    // Graph line
    nvgBeginPath(vg);
    nvgMoveTo(vg, sx[0], sy[0]+2);
    for (i = 1; i < 6; i++)
        nvgBezierTo(vg, sx[i-1]+dx*0.5f,sy[i-1]+2, sx[i]-dx*0.5f,sy[i]+2, sx[i],sy[i]+2);
    nvgStrokeColor(vg, nvgRGBA(0,0,0,32));
    nvgStrokeWidth(vg, 3.0f);
    nvgStroke(vg);

    nvgBeginPath(vg);
    nvgMoveTo(vg, sx[0], sy[0]);
    for (i = 1; i < 6; i++)
        nvgBezierTo(vg, sx[i-1]+dx*0.5f,sy[i-1], sx[i]-dx*0.5f,sy[i], sx[i],sy[i]);
    nvgStrokeColor(vg, nvgRGBA(0,160,192,255));
    nvgStrokeWidth(vg, 3.0f);
    nvgStroke(vg);

    // Graph sample pos
    for (i = 0; i < 6; i++) {
        bg = nvgRadialGradient(vg, sx[i],sy[i]+2, 3.0f,8.0f, nvgRGBA(0,0,0,32), nvgRGBA(0,0,0,0));
        nvgBeginPath(vg);
        nvgRect(vg, sx[i]-10, sy[i]-10+2, 20,20);
        nvgFillPaint(vg, bg);
        nvgFill(vg);
    }

    nvgBeginPath(vg);
    for (i = 0; i < 6; i++)
        nvgCircle(vg, sx[i], sy[i], 4.0f);
    nvgFillColor(vg, nvgRGBA(0,160,192,255));
    nvgFill(vg);
    nvgBeginPath(vg);
    for (i = 0; i < 6; i++)
        nvgCircle(vg, sx[i], sy[i], 2.0f);
    nvgFillColor(vg, nvgRGBA(220,220,220,255));
    nvgFill(vg);

    nvgStrokeWidth(vg, 1.0f);
}

// called on every frame outside the render pass
void prepareRenderTest(QRhiCommandBuffer *cb, QRhiRenderTarget *rt, const QPointF &mousePos)
{
    vg.begin(cb, rt);

    nvgBeginPath(vg.ctx);
    nvgRect(vg.ctx, 10, 10, 200, 200);
    nvgFillColor(vg.ctx, nvgRGBA(255, 0, 0, 255));
    nvgFill(vg.ctx);

    nvgFontFace(vg.ctx, "font");
    nvgFontSize(vg.ctx, 36.0f);
    nvgFillColor(vg.ctx, nvgRGBA(220, 0, 220, 255));
    nvgText(vg.ctx, 10, 300, "hello world", nullptr);


    float x = 300;
    float y = 10;
    float w = 500;
    float h = 500;
    float cornerRadius = 10.0;
    // Window
    nvgBeginPath(vg.ctx);
    nvgRoundedRect(vg.ctx, x,y, w,h, cornerRadius);
    nvgFillColor(vg.ctx, nvgRGBA(28,30,34,192));
    //	nvgFillColor(vg.ctx, nvgRGBA(0,0,0,128));
    nvgFill(vg.ctx);

    // Drop shadow
    NVGpaint  shadowPaint = nvgBoxGradient(vg.ctx, x,y+2, w,h, cornerRadius*2, 10, nvgRGBA(0,0,0,128), nvgRGBA(0,0,0,0));
    nvgBeginPath(vg.ctx);
    nvgRect(vg.ctx, x-10,y-10, w+20,h+30);
    nvgRoundedRect(vg.ctx, x,y, w,h, cornerRadius);
    nvgPathWinding(vg.ctx, NVG_HOLE);
    nvgFillPaint(vg.ctx, shadowPaint);
    nvgFill(vg.ctx);

    // Header
    NVGpaint  headerPaint = nvgLinearGradient(vg.ctx, x,y,x,y+15, nvgRGBA(255,255,255,8), nvgRGBA(0,0,0,16));
    nvgBeginPath(vg.ctx);
    nvgRoundedRect(vg.ctx, x+1,y+1, w-2,30, cornerRadius-1);
    nvgFillPaint(vg.ctx, headerPaint);
    nvgFill(vg.ctx);
    nvgBeginPath(vg.ctx);
    nvgMoveTo(vg.ctx, x+0.5f, y+0.5f+30);
    nvgLineTo(vg.ctx, x+0.5f+w-1, y+0.5f+30);
    nvgStrokeColor(vg.ctx, nvgRGBA(0,0,0,32));
    nvgStroke(vg.ctx);

    nvgFontSize(vg.ctx, 15.0f);
    nvgFontFace(vg.ctx, "font");
    nvgTextAlign(vg.ctx,NVG_ALIGN_CENTER|NVG_ALIGN_MIDDLE);

    nvgFontBlur(vg.ctx, 2);
    nvgFillColor(vg.ctx, nvgRGBA(0,0,0,128));
    nvgText(vg.ctx, x+w/2,y+16+1, "title", NULL);

    nvgFontBlur(vg.ctx,0);
    nvgFillColor(vg.ctx, nvgRGBA(220,220,220,160));
    nvgText(vg.ctx, x+w/2,y+16, "title", NULL);

    drawEyes(vg.ctx, 600, 600, 120, 120, mousePos.x(), mousePos.y(), 1);
    drawGraph(vg.ctx, 200, 500, 100, 100, 1.0);

    vg.end();
}

// called one very frame inside the render pass
void renderTest()
{
    vg.render();
}

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

struct Window : public QWindow
{
    Window(QRhi::Implementation graphicsApi);

    void releaseSwapChain();

#if QT_CONFIG(opengl)
    std::unique_ptr<QOffscreenSurface> m_fallbackSurface;
#endif
    std::unique_ptr<QRhi> m_rhi;
    std::unique_ptr<QRhiSwapChain> m_sc;
    std::unique_ptr<QRhiRenderBuffer> m_ds;
    std::unique_ptr<QRhiRenderPassDescriptor> m_rp;

    bool m_hasSwapChain = false;
    QMatrix4x4 m_proj;

    void init();
    void resizeSwapChain();
    void render();

    void exposeEvent(QExposeEvent *) override;
    bool event(QEvent *) override;
    void mouseMoveEvent(QMouseEvent *) override;

    QRhi::Implementation m_graphicsApi;

    bool m_running = false;
    bool m_notExposed = false;
    bool m_newlyExposed = false;

    std::unique_ptr<QRhiBuffer> m_vbuf;
    bool m_vbufReady = false;
    std::unique_ptr<QRhiBuffer> m_ubuf;
    std::unique_ptr<QRhiShaderResourceBindings> m_srb;
    std::unique_ptr<QRhiGraphicsPipeline> m_ps;

    float m_rotation = 0;
    float m_opacity = 1;
    int m_opacityDir = -1;
    QPointF m_mousePos;
};

Window::Window(QRhi::Implementation graphicsApi)
    : m_graphicsApi(graphicsApi)
{
    switch (graphicsApi) {
    case QRhi::OpenGLES2:
        setSurfaceType(OpenGLSurface);
        break;
    case QRhi::Vulkan:
        setSurfaceType(VulkanSurface);
        break;
    case QRhi::D3D11:
#if QT_VERSION_MAJOR > 6 || QT_VERSION_MINOR >= 6
    case QRhi::D3D12:
#endif
        setSurfaceType(Direct3DSurface);
        break;
    case QRhi::Metal:
        setSurfaceType(MetalSurface);
        break;
    default:
        break;
    }
}

void Window::exposeEvent(QExposeEvent *)
{
    if (isExposed() && !m_running) {
        m_running = true;
        init();
        resizeSwapChain();
    }

    const QSize surfaceSize = m_hasSwapChain ? m_sc->surfacePixelSize() : QSize();

    if ((!isExposed() || (m_hasSwapChain && surfaceSize.isEmpty())) && m_running && !m_notExposed)
        m_notExposed = true;

    if (isExposed() && m_running && m_notExposed && !surfaceSize.isEmpty()) {
        m_notExposed = false;
        m_newlyExposed = true;
    }

    if (isExposed() && !surfaceSize.isEmpty())
        render();
}

bool Window::event(QEvent *e)
{
    switch (e->type()) {
    case QEvent::UpdateRequest:
        render();
        break;

    case QEvent::PlatformSurface:
        if (static_cast<QPlatformSurfaceEvent *>(e)->surfaceEventType() == QPlatformSurfaceEvent::SurfaceAboutToBeDestroyed) {
            cleanupTest();
            releaseSwapChain();
        }
        break;

    default:
        break;
    }

    return QWindow::event(e);
}

void Window::mouseMoveEvent(QMouseEvent *e)
{
    m_mousePos = e->scenePosition();
}

void Window::init()
{
    QRhi::Flags rhiFlags = QRhi::EnableDebugMarkers
#if QT_VERSION_MAJOR > 6 || QT_VERSION_MINOR >= 6
                           | QRhi::EnableTimestamps
#else
                           | QRhi::EnableProfiling
#endif
        ;

    if (m_graphicsApi == QRhi::Null) {
        QRhiNullInitParams params;
        m_rhi.reset(QRhi::create(QRhi::Null, &params, rhiFlags));
    }

#if QT_CONFIG(opengl)
    if (m_graphicsApi == QRhi::OpenGLES2) {
        m_fallbackSurface.reset(QRhiGles2InitParams::newFallbackSurface());
        QRhiGles2InitParams params;
        params.fallbackSurface = m_fallbackSurface.get();
        params.window = this;
        m_rhi.reset(QRhi::create(QRhi::OpenGLES2, &params, rhiFlags));
    }
#endif

#if QT_CONFIG(vulkan)
    if (m_graphicsApi == QRhi::Vulkan) {
        QRhiVulkanInitParams params;
        params.inst = vulkanInstance();
        params.window = this;
        m_rhi.reset(QRhi::create(QRhi::Vulkan, &params, rhiFlags));
    }
#endif

#ifdef Q_OS_WIN
    if (m_graphicsApi == QRhi::D3D11) {
        QRhiD3D11InitParams params;
        params.enableDebugLayer = true;
        m_rhi.reset(QRhi::create(QRhi::D3D11, &params, rhiFlags));
    }
#if QT_VERSION_MAJOR > 6 || QT_VERSION_MINOR >= 6
    else if (m_graphicsApi == QRhi::D3D12) {
        QRhiD3D12InitParams params;
        params.enableDebugLayer = true;
        m_rhi.reset(QRhi::create(QRhi::D3D12, &params, rhiFlags));
    }
#endif
#endif

#if defined(Q_OS_MACOS) || defined(Q_OS_IOS)
    if (m_graphicsApi == QRhi::Metal) {
        QRhiMetalInitParams params;
        m_rhi.reset(QRhi::create(QRhi::Metal, &params, rhiFlags));
    }
#endif

    if (!m_rhi)
        qFatal("Failed to create RHI backend");

    m_sc.reset(m_rhi->newSwapChain());
    m_ds.reset(m_rhi->newRenderBuffer(QRhiRenderBuffer::DepthStencil,
                                      QSize(),
                                      1,
                                      QRhiRenderBuffer::UsedWithSwapChainOnly));
    m_sc->setWindow(this);
    m_sc->setDepthStencil(m_ds.get());
    m_rp.reset(m_sc->newCompatibleRenderPassDescriptor());
    m_sc->setRenderPassDescriptor(m_rp.get());

    m_vbuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::VertexBuffer, sizeof(vertexData)));
    m_vbuf->create();
    m_vbufReady = false;

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

    const QShader vs = getShader(QLatin1String(":/shaders/color.vert.qsb"));
    if (!vs.isValid())
        qFatal("Failed to load shader pack (vertex)");
    const QShader fs = getShader(QLatin1String(":/shaders/color.frag.qsb"));
    if (!fs.isValid())
        qFatal("Failed to load shader pack (fragment)");

    m_ps->setShaderStages({
        { QRhiShaderStage::Vertex, vs },
        { QRhiShaderStage::Fragment, fs }
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
    m_ps->setRenderPassDescriptor(m_rp.get());

    m_ps->create();

    initTest(m_rhi.get());
}

void Window::resizeSwapChain()
{
    m_hasSwapChain = m_sc->createOrResize();

    const QSize outputSize = m_sc->currentPixelSize();
    m_proj = m_rhi->clipSpaceCorrMatrix();
    m_proj.perspective(45.0f, outputSize.width() / (float) outputSize.height(), 0.01f, 1000.0f);
    m_proj.translate(0, 0, -4);
}

void Window::releaseSwapChain()
{
    if (m_hasSwapChain) {
        m_hasSwapChain = false;
        m_sc->destroy();
    }
}

void Window::render()
{
    if (!m_hasSwapChain || m_notExposed)
        return;

    if (m_sc->currentPixelSize() != m_sc->surfacePixelSize() || m_newlyExposed) {
        resizeSwapChain();
        if (!m_hasSwapChain)
            return;
        m_newlyExposed = false;
    }

    QRhi::FrameOpResult r = m_rhi->beginFrame(m_sc.get());
    if (r == QRhi::FrameOpSwapChainOutOfDate) {
        resizeSwapChain();
        if (!m_hasSwapChain)
            return;
        r = m_rhi->beginFrame(m_sc.get());
    }
    if (r != QRhi::FrameOpSuccess) {
        qDebug("beginFrame failed with %d, retry", r);
        requestUpdate();
        return;
    }

    QRhiCommandBuffer *cb = m_sc->currentFrameCommandBuffer();
    QRhiRenderTarget *rt = m_sc->currentFrameRenderTarget();
    const QSize outputSizeInPixels = m_sc->currentPixelSize();

    QRhiResourceUpdateBatch *u = m_rhi->nextResourceUpdateBatch();
    if (!m_vbufReady) {
        m_vbufReady = true;
        u->uploadStaticBuffer(m_vbuf.get(), vertexData);
    }
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

    m_rhi->makeThreadLocalNativeContextCurrent();
    prepareRenderTest(cb, rt, m_mousePos);

    cb->beginPass(rt, QColor::fromRgbF(0.4f, 0.7f, 0.0f, 1.0f), { 1.0f, 0 }, u);

    cb->setGraphicsPipeline(m_ps.get());
    cb->setViewport({ 0, 0, float(outputSizeInPixels.width()), float(outputSizeInPixels.height()) });
    cb->setShaderResources();

    const QRhiCommandBuffer::VertexInput vbufBinding(m_vbuf.get(), 0);
    cb->setVertexInput(0, 1, &vbufBinding);
    cb->draw(3);

    renderTest();

    cb->endPass();

    m_rhi->endFrame(m_sc.get());

    requestUpdate();
}

static QString graphicsApiName(QRhi::Implementation graphicsApi)
{
    switch (graphicsApi) {
    case QRhi::Null:
        return QLatin1String("Null (no output)");
    case QRhi::OpenGLES2:
        return QLatin1String("OpenGL");
    case QRhi::Vulkan:
        return QLatin1String("Vulkan");
    case QRhi::D3D11:
        return QLatin1String("Direct3D 11");
#if QT_VERSION_MAJOR > 6 || QT_VERSION_MINOR >= 6
    case QRhi::D3D12:
        return QLatin1String("Direct3D 12");
#endif
    case QRhi::Metal:
        return QLatin1String("Metal");
    default:
        break;
    }
    return QString();
}

int main(int argc, char **argv)
{
    QGuiApplication app(argc, argv);

    QRhi::Implementation graphicsApi;
#if defined(Q_OS_WIN)
    graphicsApi = QRhi::D3D11;
#elif defined(Q_OS_MACOS) || defined(Q_OS_IOS)
    graphicsApi = QRhi::Metal;
#elif QT_CONFIG(vulkan)
    graphicsApi = QRhi::Vulkan;
#else
    graphicsApi = QRhi::OpenGLES2;
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
#if QT_VERSION_MAJOR > 6 || QT_VERSION_MINOR >= 6
    QCommandLineOption d3d12Option({ "D", "d3d12" }, QLatin1String("Direct3D 12"));
    cmdLineParser.addOption(d3d12Option);
#endif
    QCommandLineOption mtlOption({ "m", "metal" }, QLatin1String("Metal"));
    cmdLineParser.addOption(mtlOption);

    cmdLineParser.process(app);
    if (cmdLineParser.isSet(nullOption))
        graphicsApi = QRhi::Null;
    if (cmdLineParser.isSet(glOption))
        graphicsApi = QRhi::OpenGLES2;
    if (cmdLineParser.isSet(vkOption))
        graphicsApi = QRhi::Vulkan;
    if (cmdLineParser.isSet(d3d11Option))
        graphicsApi = QRhi::D3D11;
#if QT_VERSION_MAJOR > 6 || QT_VERSION_MINOR >= 6
    if (cmdLineParser.isSet(d3d12Option))
        graphicsApi = QRhi::D3D12;
#endif
    if (cmdLineParser.isSet(mtlOption))
        graphicsApi = QRhi::Metal;

    qDebug("Selected graphics API is %s", qPrintable(graphicsApiName(graphicsApi)));
    qDebug("This is a multi-api example, use command line arguments to override:\n%s", qPrintable(cmdLineParser.helpText()));

    QSurfaceFormat fmt;
    fmt.setDepthBufferSize(24);
    fmt.setStencilBufferSize(8);
    QSurfaceFormat::setDefaultFormat(fmt);

#if QT_CONFIG(vulkan)
    QVulkanInstance inst;
    if (graphicsApi == QRhi::Vulkan) {
        inst.setLayers({ "VK_LAYER_KHRONOS_validation" });
        inst.setExtensions(QRhiVulkanInitParams::preferredInstanceExtensions());
        if (!inst.create()) {
            qWarning("Failed to create Vulkan instance, switching to OpenGL");
            graphicsApi = QRhi::OpenGLES2;
        }
    }
#endif

    Window w(graphicsApi);
#if QT_CONFIG(vulkan)
    if (graphicsApi == QRhi::Vulkan)
        w.setVulkanInstance(&inst);
#endif
    w.resize(1280, 720);
    w.setTitle(QCoreApplication::applicationName() + QLatin1String(" - ") + graphicsApiName(graphicsApi));
    w.show();

    int ret = app.exec();

    if (w.handle())
        w.releaseSwapChain();

    return ret;
}

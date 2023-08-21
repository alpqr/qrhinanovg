//
// Copyright (c) 2009-2013 Mikko Mononen memon@inside.org
//
// This software is provided 'as-is', without any express or implied
// warranty.  In no event will the authors be held liable for any damages
// arising from the use of this software.
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
//

#include "nanovg_rhi.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <QFile>
#include <rhi/qrhi.h>

// ###
#include <QOpenGLExtraFunctions>

QT_BEGIN_NAMESPACE

enum GLNVGshaderType {
    NSVG_SHADER_FILLGRAD,
    NSVG_SHADER_FILLIMG,
    NSVG_SHADER_SIMPLE,
    NSVG_SHADER_IMG
};

struct GLNVGtexture {
    int id;
    GLuint tex;
    QRhiTexture *rhiTex;
    int width;
    int height;
    int type;
    int flags;
};

struct GLNVGblend
{
    QRhiGraphicsPipeline::BlendFactor srcRGB;
    QRhiGraphicsPipeline::BlendFactor dstRGB;
    QRhiGraphicsPipeline::BlendFactor srcAlpha;
    QRhiGraphicsPipeline::BlendFactor dstAlpha;
};

enum GLNVGcallType {
    GLNVG_NONE = 0,
    GLNVG_FILL,
    GLNVG_CONVEXFILL,
    GLNVG_STROKE,
    GLNVG_TRIANGLES,
};

struct GLNVGcall {
    int type;
    int image;
    int pathOffset;
    int pathCount;
    int indexOffset;
    int indexCount;
    int triangleOffset;
    int triangleCount;
    int fragmentUniformBufferOffset;
    int fragmentUniformBufferIndex;
    GLNVGblend blendFunc;
    QRhiShaderResourceBindings *srb[2];
    QRhiGraphicsPipeline *ps[4];
};

struct GLNVGpath {
    int fillOffset;
    int fillCount;
    int strokeOffset;
    int strokeCount;
};

struct GLNVGfragUniforms {
    float scissorMat[12]; // matrices are actually 3 vec4s
    float paintMat[12];
    struct NVGcolor innerCol;
    struct NVGcolor outerCol;
    float scissorExt[2];
    float scissorScale[2];
    float extent[2];
    float radius;
    float feather;
    float strokeMult;
    float strokeThr;
    int texType;
    int type;
    float viewSize[2];
};

struct SamplerDesc
{
    QRhiSampler::Filter minFilter;
    QRhiSampler::Filter magFilter;
    QRhiSampler::Filter mipmap;
    QRhiSampler::AddressMode hTiling;
    QRhiSampler::AddressMode vTiling;
    QRhiSampler::AddressMode zTiling;
};

inline bool operator==(const SamplerDesc &a, const SamplerDesc &b) Q_DECL_NOTHROW
{
    return a.hTiling == b.hTiling && a.vTiling == b.vTiling && a.zTiling == b.zTiling
           && a.minFilter == b.minFilter && a.magFilter == b.magFilter
           && a.mipmap == b.mipmap;
}

struct PipelineState
{
    bool edgeAA = false;

    QRhiGraphicsPipeline::Topology topology = QRhiGraphicsPipeline::Triangles;

    QRhiGraphicsPipeline::CullMode cullMode = QRhiGraphicsPipeline::Back;

    bool depthTestEnable = false;
    bool depthWriteEnable = false;
    QRhiGraphicsPipeline::CompareOp depthFunc = QRhiGraphicsPipeline::LessOrEqual;

    bool stencilTestEnable = false;
    bool usesStencilRef = false;
    QRhiGraphicsPipeline::StencilOpState stencilFront;
    QRhiGraphicsPipeline::StencilOpState stencilBack;
    quint32 stencilReadMask = 0xFFFFFFFF;
    quint32 stencilWriteMask = 0xFFFFFFFF;

    bool blendEnable = true;
    QRhiGraphicsPipeline::TargetBlend targetBlend;

    int samples = 1;
};

inline bool operator==(const PipelineState &a, const PipelineState &b) Q_DECL_NOTHROW
{
    return a.edgeAA == b.edgeAA
           && a.topology == b.topology
           && a.cullMode == b.cullMode
           && a.depthTestEnable == b.depthTestEnable
           && a.depthWriteEnable == b.depthWriteEnable
           && a.depthFunc == b.depthFunc
           && a.stencilTestEnable == b.stencilTestEnable
           && a.usesStencilRef == b.usesStencilRef
           && a.stencilFront.failOp == b.stencilFront.failOp
           && a.stencilFront.depthFailOp == b.stencilFront.depthFailOp
           && a.stencilFront.passOp == b.stencilFront.passOp
           && a.stencilFront.compareOp == b.stencilFront.compareOp
           && a.stencilBack.failOp == b.stencilBack.failOp
           && a.stencilBack.depthFailOp == b.stencilBack.depthFailOp
           && a.stencilBack.passOp == b.stencilBack.passOp
           && a.stencilBack.compareOp == b.stencilBack.compareOp
           && a.stencilReadMask == b.stencilReadMask
           && a.stencilWriteMask == b.stencilWriteMask
           && a.blendEnable == b.blendEnable
           // NB! not memcmp
           && a.targetBlend.colorWrite == b.targetBlend.colorWrite
           && a.targetBlend.srcColor == b.targetBlend.srcColor
           && a.targetBlend.dstColor == b.targetBlend.dstColor
           && a.targetBlend.opColor == b.targetBlend.opColor
           && a.targetBlend.srcAlpha == b.targetBlend.srcAlpha
           && a.targetBlend.dstAlpha == b.targetBlend.dstAlpha
           && a.targetBlend.opAlpha == b.targetBlend.opAlpha
           && a.samples == b.samples;
}

inline bool operator!=(const PipelineState &a, const PipelineState &b) Q_DECL_NOTHROW
{
    return !(a == b);
}

inline size_t qHash(const PipelineState &s, size_t seed) Q_DECL_NOTHROW
{
    // do not bother with all fields
    return qHash(s.edgeAA, seed)
           ^ qHash(s.samples)
           ^ qHash(s.targetBlend.dstColor)
           ^ qHash(s.depthFunc)
           ^ qHash(s.cullMode)
           ^ qHashBits(&s.stencilFront, sizeof(QRhiGraphicsPipeline::StencilOpState))
           ^ (s.depthTestEnable << 1)
           ^ (s.depthWriteEnable << 2)
           ^ (s.stencilTestEnable << 3)
           ^ (s.blendEnable << 4)
           ^ (s.usesStencilRef << 5);
}

struct PipelineStateKey
{
    PipelineState state;
    QVector<quint32> renderTargetDescription;
    QVector<quint32> srbLayoutDescription;
    struct {
        size_t renderTargetDescriptionHash;
        size_t srbLayoutDescriptionHash;
    } extra;
    static PipelineStateKey create(const PipelineState &state,
                                   const QRhiRenderPassDescriptor *rpDesc,
                                   const QRhiShaderResourceBindings *srb)
    {
        const QVector<quint32> rtDesc = rpDesc->serializedFormat();
        const QVector<quint32> srbDesc = srb->serializedLayoutDescription();
        return { state, rtDesc, srbDesc, { qHash(rtDesc), qHash(srbDesc) } };
    }
};

inline bool operator==(const PipelineStateKey &a, const PipelineStateKey &b) Q_DECL_NOTHROW
{
    return a.state == b.state
           && a.renderTargetDescription == b.renderTargetDescription
           && a.srbLayoutDescription == b.srbLayoutDescription;
}

inline bool operator!=(const PipelineStateKey &a, const PipelineStateKey &b) Q_DECL_NOTHROW
{
    return !(a == b);
}

inline size_t qHash(const PipelineStateKey &k, size_t seed) Q_DECL_NOTHROW
{
    return qHash(k.state, seed)
           ^ k.extra.renderTargetDescriptionHash
           ^ k.extra.srbLayoutDescriptionHash;
}

static QShader getShader(const QString &name)
{
    QFile f(name);
    return f.open(QIODevice::ReadOnly) ? QShader::fromSerialized(f.readAll()) : QShader();
}

struct Shaders
{
    Shaders() {
        vs = getShader(QLatin1String(":/nanovg_shaders/fill.vert.qsb"));
        fs = getShader(QLatin1String(":/nanovg_shaders/fill.frag.qsb"));
        fsAA = getShader(QLatin1String(":/nanovg_shaders/fill_edgeaa.frag.qsb"));

        vertexInputLayout.setBindings({
            { 4 * sizeof(float) }
        });
        vertexInputLayout.setAttributes({
            { 0, 0, QRhiVertexInputAttribute::Float2, 0 },
            { 0, 1, QRhiVertexInputAttribute::Float2, 2 * sizeof(float) }
        });
    }

    QShader vs;
    QShader fs;
    QShader fsAA; // EDGE_AA enabled
    QRhiVertexInputLayout vertexInputLayout;
};

struct RhiNvgContext
{
    QRhi *rhi = nullptr;
    QRhiCommandBuffer *cb = nullptr;
    QRhiRenderTarget *rt = nullptr;

    GLuint prog = 0;
    GLuint frag = 0;
    GLuint vert = 0;
    GLint texLoc = 0;

    GLNVGtexture* textures = nullptr;
    float view[2] = {};
    int ntextures = 0;
    int ctextures = 0;
    int textureId = 0;
    GLuint vertBuf = 0;
    GLuint indexBuffer = 0;
    GLuint fragmentUniformBuffer = 0;
    GLuint vertexUniformBuffer = 0;
    int oneFragmentUniformBufferSize = 0;
    int flags = 0;

    // Per frame buffers
    GLNVGcall* calls = nullptr;
    int ccalls = 0;
    int ncalls = 0;
    GLNVGpath* paths = nullptr;
    int cpaths = 0;
    int npaths = 0;
    struct NVGvertex* verts = nullptr;
    int cverts = 0;
    int nverts = 0;
    uint32_t *indices = nullptr;
    int cindices = 0;
    int nindices = 0;
    unsigned char* uniforms = nullptr;
    int cuniforms = 0;
    int nuniforms = 0;

    int dummyTex = 0;

    Shaders shaders;
    QVector<std::pair<SamplerDesc, QRhiSampler*>> samplers;
    QHash<PipelineStateKey, QRhiGraphicsPipeline *> pipelines;
    QRhiBuffer *rhiVertexBuffer = nullptr;
    QRhiBuffer *rhiIndexBuffer = nullptr;
    QRhiBuffer *rhiVSUniformBuffer = nullptr;
    QRhiBuffer *rhiFSUniformBuffer = nullptr;
    QRhiResourceUpdateBatch *resourceUpdates = nullptr;
    // The uniform buffers are the same for every draw call (the fragment
    // uniform buffer uses dynamic offsets), only the QRhiTexture and Sampler
    // can be different. The image flags (which defines the QRhiSampler) are
    // static once an image_id is created, so a simple image_id -> srb mapping
    // is possible.
    QHash<int, QRhiShaderResourceBindings *> srbs;
};

static QRhiGraphicsPipeline *pipeline(RhiNvgContext *rc,
                                      const PipelineStateKey &key,
                                      QRhiRenderPassDescriptor *rpDesc,
                                      QRhiShaderResourceBindings *srb)
{
    auto it = rc->pipelines.constFind(key);
    if (it != rc->pipelines.constEnd())
        return it.value();

    QRhiGraphicsPipeline *ps = rc->rhi->newGraphicsPipeline();

    ps->setShaderStages({
        { QRhiShaderStage::Vertex, rc->shaders.vs },
        { QRhiShaderStage::Fragment, key.state.edgeAA ? rc->shaders.fsAA : rc->shaders.fs }
    });
    ps->setVertexInputLayout(rc->shaders.vertexInputLayout);
    ps->setShaderResourceBindings(srb);
    ps->setRenderPassDescriptor(rpDesc);

    QRhiGraphicsPipeline::Flags flags;
    if (key.state.usesStencilRef)
        flags |= QRhiGraphicsPipeline::UsesStencilRef;
    ps->setFlags(flags);

    ps->setTopology(key.state.topology);
    ps->setCullMode(key.state.cullMode);

    QRhiGraphicsPipeline::TargetBlend blend = key.state.targetBlend;
    blend.enable = key.state.blendEnable;
    const int colorAttachmentCount = 1;
    QVarLengthArray<QRhiGraphicsPipeline::TargetBlend, 8> targetBlends(colorAttachmentCount);
    for (int i = 0; i < colorAttachmentCount; ++i)
        targetBlends[i] = blend;
    ps->setTargetBlends(targetBlends.cbegin(), targetBlends.cend());

    ps->setSampleCount(key.state.samples);

    ps->setDepthTest(key.state.depthTestEnable);
    ps->setDepthWrite(key.state.depthWriteEnable);
    ps->setDepthOp(key.state.depthFunc);

    ps->setStencilTest(key.state.stencilTestEnable);
    ps->setStencilFront(key.state.stencilFront);
    ps->setStencilBack(key.state.stencilBack);
    ps->setStencilReadMask(key.state.stencilReadMask);
    ps->setStencilWriteMask(key.state.stencilWriteMask);

    if (!ps->create()) {
        qWarning("Failed to build graphics pipeline state");
        delete ps;
        return nullptr;
    }

    rc->pipelines.insert(key, ps);
    return ps;
}

static QRhiSampler *sampler(RhiNvgContext *rc, const SamplerDesc &samplerDescription)
{
    auto compareSampler = [samplerDescription](const std::pair<SamplerDesc, QRhiSampler*> &info) {
        return info.first == samplerDescription;
    };
    const auto found = std::find_if(rc->samplers.cbegin(), rc->samplers.cend(), compareSampler);
    if (found != rc->samplers.cend())
        return found->second;

    QRhiSampler *newSampler = rc->rhi->newSampler(samplerDescription.magFilter,
                                                  samplerDescription.minFilter,
                                                  samplerDescription.mipmap,
                                                  samplerDescription.hTiling,
                                                  samplerDescription.vTiling,
                                                  samplerDescription.zTiling);
    if (!newSampler->create()) {
        qWarning("Failed to build image sampler");
        delete newSampler;
        return nullptr;
    }
    rc->samplers << std::make_pair(samplerDescription, newSampler);
    return newSampler;
}

static bool ensureBufferCapacity(QRhiBuffer **buf, quint32 size)
{
    if ((*buf)->size() < size) {
        (*buf)->setSize(size);
        if (!(*buf)->create()) {
            qWarning("Failed to recreate buffer with size %u", size);
            return false;
        }
    }
    return true;
}

static QRhiResourceUpdateBatch *resourceUpdateBatch(RhiNvgContext *rc)
{
    if (!rc->resourceUpdates)
        rc->resourceUpdates = rc->rhi->nextResourceUpdateBatch();
    return rc->resourceUpdates;
}

static void commitResourceUpdates(RhiNvgContext *rc)
{
    if (rc->resourceUpdates) {
        rc->cb->resourceUpdate(rc->resourceUpdates);
        rc->resourceUpdates = nullptr;
    }
}

static GLNVGtexture *findTexture(RhiNvgContext *rc, int id)
{
    for (int i = 0; i < rc->ntextures; i++) {
        if (rc->textures[i].id == id)
            return &rc->textures[i];
    }
    return nullptr;
}

static QRhiShaderResourceBindings *createSrb(RhiNvgContext *rc, int image)
{
    GLNVGtexture* tex = NULL;
    if (image != 0) {
        tex = findTexture(rc, image);
    }
    // If no image is set, use empty texture
    if (tex == NULL) {
        tex = findTexture(rc, rc->dummyTex);
    }

    if (tex && tex->rhiTex) {
        SamplerDesc samplerDesc;
        samplerDesc.minFilter = (tex->flags & NVG_IMAGE_NEAREST) ? QRhiSampler::Nearest : QRhiSampler::Linear;
        samplerDesc.magFilter = (tex->flags & NVG_IMAGE_NEAREST) ? QRhiSampler::Nearest : QRhiSampler::Linear;
        samplerDesc.mipmap = (tex->flags & NVG_IMAGE_GENERATE_MIPMAPS) ? ((tex->flags & NVG_IMAGE_NEAREST) ? QRhiSampler::Nearest : QRhiSampler::Linear) : QRhiSampler::None;
        samplerDesc.hTiling = (tex->flags & NVG_IMAGE_REPEATX) ? QRhiSampler::Repeat : QRhiSampler::ClampToEdge;
        samplerDesc.vTiling = (tex->flags & NVG_IMAGE_REPEATY) ? QRhiSampler::Repeat : QRhiSampler::ClampToEdge;
        samplerDesc.zTiling = QRhiSampler::Repeat;
        QRhiShaderResourceBindings *srb = rc->rhi->newShaderResourceBindings();
        srb->setBindings({
            QRhiShaderResourceBinding::uniformBuffer(0, QRhiShaderResourceBinding::VertexStage, rc->rhiVSUniformBuffer),
            QRhiShaderResourceBinding::uniformBufferWithDynamicOffset(1, QRhiShaderResourceBinding::FragmentStage, rc->rhiFSUniformBuffer, sizeof(GLNVGfragUniforms)),
            QRhiShaderResourceBinding::sampledTexture(2, QRhiShaderResourceBinding::FragmentStage, tex->rhiTex, sampler(rc, samplerDesc))
        });
        if (!srb->create()) {
            qWarning("Failed to create resource bindings");
            delete srb;
            return nullptr;
        }
        return srb;
    }

    return nullptr;
}

#define QGL QOpenGLContext::currentContext()->extraFunctions()

static void glnvg__dumpShaderError(GLuint shader, const char* name, const char* type)
{
    GLchar str[512+1];
    GLsizei len = 0;
    QGL->glGetShaderInfoLog(shader, 512, &len, str);
    if (len > 512) len = 512;
    str[len] = '\0';
    qDebug("Shader %s/%s error:\n%s\n", name, type, str);
}

static void glnvg__dumpProgramError(GLuint prog, const char* name)
{
    GLchar str[512+1];
    GLsizei len = 0;
    QGL->glGetProgramInfoLog(prog, 512, &len, str);
    if (len > 512) len = 512;
    str[len] = '\0';
    qDebug("Program %s error:\n%s\n", name, str);
}

static int glnvg__createShader(GLuint *outProg, const char* vshader, const char* fshader)
{
    GLint status;
    GLuint prog, vert, frag;

    prog = QGL->glCreateProgram();
    vert = QGL->glCreateShader(GL_VERTEX_SHADER);
    frag = QGL->glCreateShader(GL_FRAGMENT_SHADER);
    QGL->glShaderSource(vert, 1, &vshader, 0);
    QGL->glShaderSource(frag, 1, &fshader, 0);

    QGL->glCompileShader(vert);
    QGL->glGetShaderiv(vert, GL_COMPILE_STATUS, &status);
    if (status != GL_TRUE) {
        glnvg__dumpShaderError(vert, "", "vert");
        return 0;
    }

    QGL->glCompileShader(frag);
    QGL->glGetShaderiv(frag, GL_COMPILE_STATUS, &status);
    if (status != GL_TRUE) {
        glnvg__dumpShaderError(frag, "", "frag");
        return 0;
    }

    QGL->glAttachShader(prog, vert);
    QGL->glAttachShader(prog, frag);

    QGL->glLinkProgram(prog);
    QGL->glGetProgramiv(prog, GL_LINK_STATUS, &status);
    if (status != GL_TRUE) {
        glnvg__dumpProgramError(prog, "");
        return 0;
    }

    *outProg = prog;

    QGL->glDeleteShader(vert);
    QGL->glDeleteShader(frag);

    return 1;
}

static int glnvg__renderCreateTexture(void* uptr, int type, int w, int h, int imageFlags, const unsigned char* data);

static const char* fillVertShader =
    "#version 440\n"
    "layout(location = 0) in vec2 vertex;\n"
    "layout(location = 1) in vec2 tcoord;\n"
    "out vec2 ftcoord;\n"
    "out vec2 fpos;\n"
    "layout(std140, binding = 0) uniform vertBuf {\n"
    "       vec2 viewSize;\n"
    "   };\n"
    "void main(void) {\n"
    "    ftcoord = tcoord;\n"
    "    fpos = vertex;\n"
    "    gl_Position = vec4(2.0*vertex.x/viewSize.x - 1.0, 1.0 - 2.0*vertex.y/viewSize.y, 0, 1);\n"
    "}\n";

static const char* fillFragShader =
    "#version 440\n"
    "layout(std140, binding = 1) uniform buf {\n"
    "    mat3 scissorMat;\n"
    "    mat3 paintMat;\n"
    "    vec4 innerCol;\n"
    "    vec4 outerCol;\n"
    "    vec2 scissorExt;\n"
    "		vec2 scissorScale;\n"
    "		vec2 extent;\n"
    "		float radius;\n"
    "		float feather;\n"
    "		float strokeMult;\n"
    "		float strokeThr;\n"
    "		int texType;\n"
    "		int type;\n"
    "   };\n"
    "	uniform sampler2D tex;\n"
    "	in vec2 ftcoord;\n"
    "	in vec2 fpos;\n"
    "	out vec4 outColor;\n"
    "\n"
    "float sdroundrect(vec2 pt, vec2 ext, float rad) {\n"
    "	vec2 ext2 = ext - vec2(rad,rad);\n"
    "	vec2 d = abs(pt) - ext2;\n"
    "	return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;\n"
    "}\n"
    "\n"
    "// Scissoring\n"
    "float scissorMask(vec2 p) {\n"
    "	vec2 sc = (abs((scissorMat * vec3(p,1.0)).xy) - scissorExt);\n"
    "	sc = vec2(0.5,0.5) - sc * scissorScale;\n"
    "	return clamp(sc.x,0.0,1.0) * clamp(sc.y,0.0,1.0);\n"
    "}\n"
    "#ifdef EDGE_AA\n"
    "// Stroke - from [0..1] to clipped pyramid, where the slope is 1px.\n"
    "float strokeMask() {\n"
    "	return min(1.0, (1.0-abs(ftcoord.x*2.0-1.0))*strokeMult) * min(1.0, ftcoord.y);\n"
    "}\n"
    "#endif\n"
    "\n"
    "void main(void) {\n"
    "   vec4 result;\n"
    "	float scissor = scissorMask(fpos);\n"
    "#ifdef EDGE_AA\n"
    "	float strokeAlpha = strokeMask();\n"
    "	if (strokeAlpha < strokeThr) discard;\n"
    "#else\n"
    "	float strokeAlpha = 1.0;\n"
    "#endif\n"
    "	if (type == 0) {			// Gradient\n"
    "		// Calculate gradient color using box gradient\n"
    "		vec2 pt = (paintMat * vec3(fpos,1.0)).xy;\n"
    "		float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5) / feather, 0.0, 1.0);\n"
    "		vec4 color = mix(innerCol,outerCol,d);\n"
    "		// Combine alpha\n"
    "		color *= strokeAlpha * scissor;\n"
    "		result = color;\n"
    "	} else if (type == 1) {		// Image\n"
    "		// Calculate color fron texture\n"
    "		vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;\n"
    "		vec4 color = texture(tex, pt);\n"
    "		if (texType == 1) color = vec4(color.xyz*color.w,color.w);"
    "		if (texType == 2) color = vec4(color.x);"
    "		// Apply color tint and alpha.\n"
    "		color *= innerCol;\n"
    "		// Combine alpha\n"
    "		color *= strokeAlpha * scissor;\n"
    "		result = color;\n"
    "	} else if (type == 2) {		// Stencil fill\n"
    "		result = vec4(1,1,1,1);\n"
    "	} else if (type == 3) {		// Textured tris\n"
    "		vec4 color = texture(tex, ftcoord);\n"
    "		if (texType == 1) color = vec4(color.xyz*color.w,color.w);"
    "		if (texType == 2) color = vec4(color.x);"
    "		color *= scissor;\n"
    "		result = color * innerCol;\n"
    "	}\n"
    "	outColor = result;\n"
    "}\n";

static const char* fillFragShaderAA =
    "#version 440\n"
    "#define EDGE_AA 1\n"
    "layout(std140, binding = 1) uniform buf {\n"
    "    mat3 scissorMat;\n"
    "    mat3 paintMat;\n"
    "    vec4 innerCol;\n"
    "    vec4 outerCol;\n"
    "    vec2 scissorExt;\n"
    "		vec2 scissorScale;\n"
    "		vec2 extent;\n"
    "		float radius;\n"
    "		float feather;\n"
    "		float strokeMult;\n"
    "		float strokeThr;\n"
    "		int texType;\n"
    "		int type;\n"
    "   };\n"
    "	uniform sampler2D tex;\n"
    "	in vec2 ftcoord;\n"
    "	in vec2 fpos;\n"
    "	out vec4 outColor;\n"
    "\n"
    "float sdroundrect(vec2 pt, vec2 ext, float rad) {\n"
    "	vec2 ext2 = ext - vec2(rad,rad);\n"
    "	vec2 d = abs(pt) - ext2;\n"
    "	return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;\n"
    "}\n"
    "\n"
    "// Scissoring\n"
    "float scissorMask(vec2 p) {\n"
    "	vec2 sc = (abs((scissorMat * vec3(p,1.0)).xy) - scissorExt);\n"
    "	sc = vec2(0.5,0.5) - sc * scissorScale;\n"
    "	return clamp(sc.x,0.0,1.0) * clamp(sc.y,0.0,1.0);\n"
    "}\n"
    "#ifdef EDGE_AA\n"
    "// Stroke - from [0..1] to clipped pyramid, where the slope is 1px.\n"
    "float strokeMask() {\n"
    "	return min(1.0, (1.0-abs(ftcoord.x*2.0-1.0))*strokeMult) * min(1.0, ftcoord.y);\n"
    "}\n"
    "#endif\n"
    "\n"
    "void main(void) {\n"
    "   vec4 result;\n"
    "	float scissor = scissorMask(fpos);\n"
    "#ifdef EDGE_AA\n"
    "	float strokeAlpha = strokeMask();\n"
    "	if (strokeAlpha < strokeThr) discard;\n"
    "#else\n"
    "	float strokeAlpha = 1.0;\n"
    "#endif\n"
    "	if (type == 0) {			// Gradient\n"
    "		// Calculate gradient color using box gradient\n"
    "		vec2 pt = (paintMat * vec3(fpos,1.0)).xy;\n"
    "		float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5) / feather, 0.0, 1.0);\n"
    "		vec4 color = mix(innerCol,outerCol,d);\n"
    "		// Combine alpha\n"
    "		color *= strokeAlpha * scissor;\n"
    "		result = color;\n"
    "	} else if (type == 1) {		// Image\n"
    "		// Calculate color fron texture\n"
    "		vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;\n"
    "		vec4 color = texture(tex, pt);\n"
    "		if (texType == 1) color = vec4(color.xyz*color.w,color.w);"
    "		if (texType == 2) color = vec4(color.x);"
    "		// Apply color tint and alpha.\n"
    "		color *= innerCol;\n"
    "		// Combine alpha\n"
    "		color *= strokeAlpha * scissor;\n"
    "		result = color;\n"
    "	} else if (type == 2) {		// Stencil fill\n"
    "		result = vec4(1,1,1,1);\n"
    "	} else if (type == 3) {		// Textured tris\n"
    "		vec4 color = texture(tex, ftcoord);\n"
    "		if (texType == 1) color = vec4(color.xyz*color.w,color.w);"
    "		if (texType == 2) color = vec4(color.x);"
    "		color *= scissor;\n"
    "		result = color * innerCol;\n"
    "	}\n"
    "	outColor = result;\n"
    "}\n";

static int glnvg__renderCreate(void* uptr)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;

    if (rc->flags & NVG_ANTIALIAS) {
        if (glnvg__createShader(&rc->prog, fillVertShader, fillFragShaderAA) == 0)
            return 0;
    } else {
        if (glnvg__createShader(&rc->prog, fillVertShader, fillFragShader) == 0)
            return 0;
    }

    rc->texLoc = QGL->glGetUniformLocation(rc->prog, "tex");

    QGL->glGenBuffers(1, &rc->vertBuf);
    QGL->glGenBuffers(1, &rc->indexBuffer);

    // Create UBOs
    QGL->glGenBuffers(1, &rc->fragmentUniformBuffer);
    QGL->glGenBuffers(1, &rc->vertexUniformBuffer);

    int ubufAlign = 4;
    QGL->glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, &ubufAlign);
    rc->oneFragmentUniformBufferSize = sizeof(GLNVGfragUniforms) + ubufAlign - sizeof(GLNVGfragUniforms) % ubufAlign;

    // Some platforms does not allow to have samples to unset textures.
    // Create empty one which is bound when there's no texture specified.
    rc->dummyTex = glnvg__renderCreateTexture(rc, NVG_TEXTURE_ALPHA, 16, 16, 0, NULL);

    rc->rhiVertexBuffer = rc->rhi->newBuffer(QRhiBuffer::Static, QRhiBuffer::VertexBuffer, 16384);
    if (!rc->rhiVertexBuffer->create()) {
        qWarning("Failed to create vertex buffer");
        return 0;
    }
    rc->rhiIndexBuffer = rc->rhi->newBuffer(QRhiBuffer::Static, QRhiBuffer::IndexBuffer, 16384);
    if (!rc->rhiIndexBuffer->create()) {
        qWarning("Failed to create index buffer");
        return 0;
    }
    rc->rhiVSUniformBuffer = rc->rhi->newBuffer(QRhiBuffer::Dynamic, QRhiBuffer::UniformBuffer, 64);
    if (!rc->rhiVSUniformBuffer->create()) {
        qWarning("Failed to create uniform buffer 0");
        return 0;
    }
    rc->rhiFSUniformBuffer = rc->rhi->newBuffer(QRhiBuffer::Dynamic, QRhiBuffer::UniformBuffer, 16384);
    if (!rc->rhiFSUniformBuffer->create()) {
        qWarning("Failed to create uniform buffer 1");
        return 0;
    }

    return 1;
}

static int glnvg__renderCreateTexture(void* uptr, int type, int w, int h, int imageFlags, const unsigned char* data)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    GLNVGtexture* tex = NULL;
    int i;

    QRhiTexture::Format format = QRhiTexture::RGBA8;
    quint32 byteSize = w * h * 4;
    if (type == NVG_TEXTURE_ALPHA) {
        format = QRhiTexture::R8; // this excludes supporting OpenGL ES 2.0 but perhaps that's fine
        byteSize = w * h;
    }

    QRhiTexture::Flags flags;
    if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS)
        flags |= QRhiTexture::MipMapped | QRhiTexture::UsedWithGenerateMips;

    QRhiTexture *t = rc->rhi->newTexture(format, QSize(w, h), 1, flags);
    if (!t->create()) {
        qWarning("Failed to create texture of size %dx%d", w, h);
        delete t;
        return 0;
    }

    for (i = 0; i < rc->ntextures; i++) {
        if (rc->textures[i].id == 0) {
            tex = &rc->textures[i];
            break;
        }
    }
    if (tex == NULL) {
        if (rc->ntextures+1 > rc->ctextures) {
            GLNVGtexture* textures;
            int ctextures = std::max(rc->ntextures+1, 4) +  rc->ctextures/2; // 1.5x Overallocate
            textures = (GLNVGtexture*)realloc(rc->textures, sizeof(GLNVGtexture)*ctextures);
            if (textures == NULL)
                return 0;
            rc->textures = textures;
            rc->ctextures = ctextures;
        }
        tex = &rc->textures[rc->ntextures++];
    }
    memset(tex, 0, sizeof(*tex));
    tex->id = ++rc->textureId;

    QGL->glGenTextures(1, &tex->tex);

    tex->width = w;
    tex->height = h;
    tex->type = type;
    tex->flags = imageFlags;
    tex->rhiTex = t;

    QRhiResourceUpdateBatch *u = resourceUpdateBatch(rc);
    if (data) {
        QRhiTextureSubresourceUploadDescription image(data, byteSize);
        QRhiTextureUploadDescription desc({ 0, 0, image });
        u->uploadTexture(tex->rhiTex, desc);
    }

    QGL->glBindTexture(GL_TEXTURE_2D, tex->tex);

    QGL->glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    QGL->glPixelStorei(GL_UNPACK_ROW_LENGTH, tex->width);
    QGL->glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
    QGL->glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);

    if (type == NVG_TEXTURE_RGBA)
        QGL->glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
    else
        QGL->glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, w, h, 0, GL_RED, GL_UNSIGNED_BYTE, data);

    if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS) {
        if (imageFlags & NVG_IMAGE_NEAREST) {
            QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
        } else {
            QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        }
    } else {
        if (imageFlags & NVG_IMAGE_NEAREST) {
            QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        } else {
            QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        }
    }

    if (imageFlags & NVG_IMAGE_NEAREST) {
        QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    } else {
        QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    }

    if (imageFlags & NVG_IMAGE_REPEATX)
        QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    else
        QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);

    if (imageFlags & NVG_IMAGE_REPEATY)
        QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    else
        QGL->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    QGL->glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
    QGL->glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
    QGL->glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
    QGL->glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);

    if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS) {
        QGL->glGenerateMipmap(GL_TEXTURE_2D);
        u->generateMips(tex->rhiTex);
    }

    return tex->id;
}

static int glnvg__renderDeleteTexture(void* uptr, int image)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;   
    int i;
    for (i = 0; i < rc->ntextures; i++) {
        if (rc->textures[i].id == image) {
            if (rc->textures[i].tex != 0 && (rc->textures[i].flags & NVG_IMAGE_NODELETE) == 0)
                QGL->glDeleteTextures(1, &rc->textures[i].tex);
            if (rc->textures[i].rhiTex && (rc->textures[i].flags & NVG_IMAGE_NODELETE) == 0)
                delete rc->textures[i].rhiTex;
            memset(&rc->textures[i], 0, sizeof(rc->textures[i]));
            return 1;
        }
    }
    return 0;
}

static int glnvg__renderUpdateTexture(void* uptr, int image, int x, int y, int w, int h, const unsigned char* data)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    GLNVGtexture* tex = findTexture(rc, image);

    if (tex == NULL) return 0;

    QRhiResourceUpdateBatch *u = resourceUpdateBatch(rc);
    if (data) {
        quint32 byteSize = w * h;
        if (tex->type != NVG_TEXTURE_ALPHA)
            byteSize *= 4;
        QRhiTextureSubresourceUploadDescription image(data, byteSize);
        image.setSourceSize(QSize(w, h));
        image.setDestinationTopLeft(QPoint(x, y));
        QRhiTextureUploadDescription desc({ 0, 0, image });
        u->uploadTexture(tex->rhiTex, desc);
    }

    QGL->glBindTexture(GL_TEXTURE_2D, tex->tex);

    QGL->glPixelStorei(GL_UNPACK_ALIGNMENT,1);

    QGL->glPixelStorei(GL_UNPACK_ROW_LENGTH, tex->width);
    QGL->glPixelStorei(GL_UNPACK_SKIP_PIXELS, x);
    QGL->glPixelStorei(GL_UNPACK_SKIP_ROWS, y);

    if (tex->type == NVG_TEXTURE_RGBA)
        QGL->glTexSubImage2D(GL_TEXTURE_2D, 0, x,y, w,h, GL_RGBA, GL_UNSIGNED_BYTE, data);
    else
        QGL->glTexSubImage2D(GL_TEXTURE_2D, 0, x,y, w,h, GL_RED, GL_UNSIGNED_BYTE, data);

    QGL->glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
    QGL->glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
    QGL->glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
    QGL->glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);

    return 1;
}

static int glnvg__renderGetTextureSize(void* uptr, int image, int* w, int* h)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    GLNVGtexture* tex = findTexture(rc, image);
    if (tex == NULL) return 0;
    *w = tex->width;
    *h = tex->height;
    return 1;
}

static void glnvg__renderViewport(void* uptr, float width, float height, float devicePixelRatio)
{
    NVG_NOTUSED(devicePixelRatio);
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    rc->view[0] = width;
    rc->view[1] = height;
}

inline QRhiGraphicsPipeline::BlendFactor glnvg_convertBlendFuncFactor(int factor)
{
    if (factor == NVG_ZERO)
        return QRhiGraphicsPipeline::Zero;
    if (factor == NVG_ONE)
        return QRhiGraphicsPipeline::One;
    if (factor == NVG_SRC_COLOR)
        return QRhiGraphicsPipeline::SrcColor;
    if (factor == NVG_ONE_MINUS_SRC_COLOR)
        return QRhiGraphicsPipeline::OneMinusSrcColor;
    if (factor == NVG_DST_COLOR)
        return QRhiGraphicsPipeline::DstColor;
    if (factor == NVG_ONE_MINUS_DST_COLOR)
        return QRhiGraphicsPipeline::OneMinusDstColor;
    if (factor == NVG_SRC_ALPHA)
        return QRhiGraphicsPipeline::SrcAlpha;
    if (factor == NVG_ONE_MINUS_SRC_ALPHA)
        return QRhiGraphicsPipeline::OneMinusSrcAlpha;
    if (factor == NVG_DST_ALPHA)
        return QRhiGraphicsPipeline::DstAlpha;
    if (factor == NVG_ONE_MINUS_DST_ALPHA)
        return QRhiGraphicsPipeline::OneMinusDstAlpha;
    if (factor == NVG_SRC_ALPHA_SATURATE)
        return QRhiGraphicsPipeline::SrcAlphaSaturate;
    return QRhiGraphicsPipeline::One;
}

inline GLNVGblend glnvg__blendCompositeOperation(NVGcompositeOperationState op)
{
    GLNVGblend blend;
    blend.srcRGB = glnvg_convertBlendFuncFactor(op.srcRGB);
    blend.dstRGB = glnvg_convertBlendFuncFactor(op.dstRGB);
    blend.srcAlpha = glnvg_convertBlendFuncFactor(op.srcAlpha);
    blend.dstAlpha = glnvg_convertBlendFuncFactor(op.dstAlpha);
    return blend;
}

static int glnvg__maxVertCount(const NVGpath* paths, int npaths, int *indexCount = nullptr)
{
    int i, count = 0;
    if (indexCount)
        *indexCount = 0;
    for (i = 0; i < npaths; i++) {
        if (paths[i].nfill > 2) {
            count += paths[i].nfill;
            if (indexCount)
                *indexCount += (paths[i].nfill - 2) * 3;
        }
        count += paths[i].nstroke;
    }
    return count;
}

static GLNVGcall* glnvg__allocCall(RhiNvgContext *rc)
{
    GLNVGcall* ret = NULL;
    if (rc->ncalls+1 > rc->ccalls) {
        GLNVGcall* calls;
        int ccalls = std::max(rc->ncalls+1, 128) + rc->ccalls/2; // 1.5x Overallocate
        calls = (GLNVGcall*)realloc(rc->calls, sizeof(GLNVGcall) * ccalls);
        if (calls == NULL) return NULL;
        rc->calls = calls;
        rc->ccalls = ccalls;
    }
    ret = &rc->calls[rc->ncalls++];
    memset(ret, 0, sizeof(GLNVGcall));
    return ret;
}

static int glnvg__allocPaths(RhiNvgContext *rc, int n)
{
    int ret = 0;
    if (rc->npaths+n > rc->cpaths) {
        GLNVGpath* paths;
        int cpaths = std::max(rc->npaths + n, 128) + rc->cpaths/2; // 1.5x Overallocate
        paths = (GLNVGpath*)realloc(rc->paths, sizeof(GLNVGpath) * cpaths);
        if (paths == NULL) return -1;
        rc->paths = paths;
        rc->cpaths = cpaths;
    }
    ret = rc->npaths;
    rc->npaths += n;
    return ret;
}

static int glnvg__allocVerts(RhiNvgContext *rc, int n)
{
    int ret = 0;
    if (rc->nverts+n > rc->cverts) {
        NVGvertex* verts;
        int cverts = std::max(rc->nverts + n, 4096) + rc->cverts/2; // 1.5x Overallocate
        verts = (NVGvertex*)realloc(rc->verts, sizeof(NVGvertex) * cverts);
        if (verts == NULL) return -1;
        rc->verts = verts;
        rc->cverts = cverts;
    }
    ret = rc->nverts;
    rc->nverts += n;
    return ret;
}

static int glnvg__allocIndices(RhiNvgContext *rc, int n)
{
    int ret = 0;
    if (rc->nindices+n > rc->cindices) {
        uint32_t *indices;
        int cindices = std::max(rc->nindices + n, 4096) + rc->cindices/2; // 1.5x Overallocate
        indices = (uint32_t*)realloc(rc->indices, sizeof(uint32_t) * cindices);
        if (indices == NULL) return -1;
        rc->indices = indices;
        rc->cindices = cindices;
    }
    ret = rc->nindices;
    rc->nindices += n;
    return ret;
}

static int glnvg__allocFragUniforms(RhiNvgContext *rc, int n)
{
    int ret = 0, structSize = rc->oneFragmentUniformBufferSize;
    if (rc->nuniforms+n > rc->cuniforms) {
        unsigned char* uniforms;
        int cuniforms = std::max(rc->nuniforms+n, 128) + rc->cuniforms/2; // 1.5x Overallocate
        uniforms = (unsigned char*)realloc(rc->uniforms, structSize * cuniforms);
        if (uniforms == NULL) return -1;
        rc->uniforms = uniforms;
        rc->cuniforms = cuniforms;
    }
    ret = rc->nuniforms * structSize;
    rc->nuniforms += n;
    return ret;
}

inline GLNVGfragUniforms* nvg__fragUniformPtr(RhiNvgContext *rc, int i)
{
    return (GLNVGfragUniforms*)&rc->uniforms[i];
}

inline void glnvg__vset(NVGvertex* vtx, float x, float y, float u, float v)
{
    vtx->x = x;
    vtx->y = y;
    vtx->u = u;
    vtx->v = v;
}

inline void glnvg__xformToMat3x4(float* m3, float* t)
{
    m3[0] = t[0];
    m3[1] = t[1];
    m3[2] = 0.0f;
    m3[3] = 0.0f;
    m3[4] = t[2];
    m3[5] = t[3];
    m3[6] = 0.0f;
    m3[7] = 0.0f;
    m3[8] = t[4];
    m3[9] = t[5];
    m3[10] = 1.0f;
    m3[11] = 0.0f;
}

inline NVGcolor glnvg__premulColor(NVGcolor c)
{
    c.r *= c.a;
    c.g *= c.a;
    c.b *= c.a;
    return c;
}

static int glnvg__convertPaint(RhiNvgContext *rc, GLNVGfragUniforms* frag, NVGpaint* paint,
                               NVGscissor* scissor, float width, float fringe, float strokeThr)
{
    GLNVGtexture* tex = NULL;
    float invxform[6];

    memset(frag, 0, sizeof(*frag));

    frag->innerCol = glnvg__premulColor(paint->innerColor);
    frag->outerCol = glnvg__premulColor(paint->outerColor);

    if (scissor->extent[0] < -0.5f || scissor->extent[1] < -0.5f) {
        memset(frag->scissorMat, 0, sizeof(frag->scissorMat));
        frag->scissorExt[0] = 1.0f;
        frag->scissorExt[1] = 1.0f;
        frag->scissorScale[0] = 1.0f;
        frag->scissorScale[1] = 1.0f;
    } else {
        nvgTransformInverse(invxform, scissor->xform);
        glnvg__xformToMat3x4(frag->scissorMat, invxform);
        frag->scissorExt[0] = scissor->extent[0];
        frag->scissorExt[1] = scissor->extent[1];
        frag->scissorScale[0] = sqrtf(scissor->xform[0]*scissor->xform[0] + scissor->xform[2]*scissor->xform[2]) / fringe;
        frag->scissorScale[1] = sqrtf(scissor->xform[1]*scissor->xform[1] + scissor->xform[3]*scissor->xform[3]) / fringe;
    }

    memcpy(frag->extent, paint->extent, sizeof(frag->extent));
    frag->strokeMult = (width*0.5f + fringe*0.5f) / fringe;
    frag->strokeThr = strokeThr;

    if (paint->image != 0) {
        tex = findTexture(rc, paint->image);
        if (tex == NULL) return 0;
        if ((tex->flags & NVG_IMAGE_FLIPY) != 0) {
            float m1[6], m2[6];
            nvgTransformTranslate(m1, 0.0f, frag->extent[1] * 0.5f);
            nvgTransformMultiply(m1, paint->xform);
            nvgTransformScale(m2, 1.0f, -1.0f);
            nvgTransformMultiply(m2, m1);
            nvgTransformTranslate(m1, 0.0f, -frag->extent[1] * 0.5f);
            nvgTransformMultiply(m1, m2);
            nvgTransformInverse(invxform, m1);
        } else {
            nvgTransformInverse(invxform, paint->xform);
        }
        frag->type = NSVG_SHADER_FILLIMG;

        if (tex->type == NVG_TEXTURE_RGBA)
            frag->texType = (tex->flags & NVG_IMAGE_PREMULTIPLIED) ? 0 : 1;
        else
            frag->texType = 2;
    } else {
        frag->type = NSVG_SHADER_FILLGRAD;
        frag->radius = paint->radius;
        frag->feather = paint->feather;
        nvgTransformInverse(invxform, paint->xform);
    }

    glnvg__xformToMat3x4(frag->paintMat, invxform);

    return 1;
}

static void glnvg__renderFill(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor, float fringe,
                              const float* bounds, const NVGpath* paths, int npaths)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    GLNVGcall* call = glnvg__allocCall(rc);
    NVGvertex* quad;
    GLNVGfragUniforms* frag;
    int i, maxverts, offset, indexOffset, indexCount;
    uint32_t *indexPtr;

    if (call == NULL) return;

    call->type = GLNVG_FILL;
    call->triangleCount = 4;
    call->pathOffset = glnvg__allocPaths(rc, npaths);
    if (call->pathOffset == -1) goto error;
    call->pathCount = npaths;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);

    if (npaths == 1 && paths[0].convex)
    {
        call->type = GLNVG_CONVEXFILL;
        call->triangleCount = 0;	// Bounding box fill quad not needed for convex fill
    }

    // Allocate vertices for all the paths.
    maxverts = glnvg__maxVertCount(paths, npaths, &indexCount) + call->triangleCount;
    offset = glnvg__allocVerts(rc, maxverts);
    if (offset == -1) goto error;

    indexOffset = glnvg__allocIndices(rc, indexCount);
    if (indexOffset == -1) goto error;
    call->indexOffset = indexOffset;
    call->indexCount = indexCount;
    indexPtr = &rc->indices[indexOffset];

    for (i = 0; i < npaths; i++) {
        GLNVGpath* copy = &rc->paths[call->pathOffset + i];
        const NVGpath* path = &paths[i];
        memset(copy, 0, sizeof(GLNVGpath));
        if (path->nfill > 2) {
            copy->fillOffset = offset;
            copy->fillCount = path->nfill;
            memcpy(&rc->verts[offset], path->fill, sizeof(NVGvertex) * path->nfill);

            int baseVertexIndex = offset;
            for (int j = 2; j < path->nfill; j++) {
                *indexPtr++ = baseVertexIndex;
                *indexPtr++ = baseVertexIndex + j - 1;
                *indexPtr++ = baseVertexIndex + j;
            }

            offset += path->nfill;
        }
        if (path->nstroke > 0) {
            copy->strokeOffset = offset;
            copy->strokeCount = path->nstroke;
            memcpy(&rc->verts[offset], path->stroke, sizeof(NVGvertex) * path->nstroke);
            offset += path->nstroke;
        }
    }

    // Setup uniforms for draw calls
    if (call->type == GLNVG_FILL) {
        // Quad
        call->triangleOffset = offset;
        quad = &rc->verts[call->triangleOffset];
        glnvg__vset(&quad[0], bounds[2], bounds[3], 0.5f, 1.0f);
        glnvg__vset(&quad[1], bounds[2], bounds[1], 0.5f, 1.0f);
        glnvg__vset(&quad[2], bounds[0], bounds[3], 0.5f, 1.0f);
        glnvg__vset(&quad[3], bounds[0], bounds[1], 0.5f, 1.0f);

        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(rc, 2);
        if (call->fragmentUniformBufferOffset == -1) goto error;
        // Simple shader for stencil
        frag = nvg__fragUniformPtr(rc, call->fragmentUniformBufferOffset);
        memset(frag, 0, sizeof(*frag));
        frag->strokeThr = -1.0f;
        frag->type = NSVG_SHADER_SIMPLE;
        // Fill shader
        glnvg__convertPaint(rc, nvg__fragUniformPtr(rc, call->fragmentUniformBufferOffset + rc->oneFragmentUniformBufferSize), paint, scissor, fringe, fringe, -1.0f);
    } else {
        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(rc, 1);
        if (call->fragmentUniformBufferOffset == -1) goto error;
        // Fill shader
        glnvg__convertPaint(rc, nvg__fragUniformPtr(rc, call->fragmentUniformBufferOffset), paint, scissor, fringe, fringe, -1.0f);
    }

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (rc->ncalls > 0) rc->ncalls--;
}

static void glnvg__renderStroke(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor, float fringe,
                                float strokeWidth, const NVGpath* paths, int npaths)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    GLNVGcall* call = glnvg__allocCall(rc);
    int i, maxverts, offset;

    if (call == NULL) return;

    call->type = GLNVG_STROKE;
    call->pathOffset = glnvg__allocPaths(rc, npaths);
    if (call->pathOffset == -1) goto error;
    call->pathCount = npaths;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);

    // Allocate vertices for all the paths.
    maxverts = glnvg__maxVertCount(paths, npaths);
    offset = glnvg__allocVerts(rc, maxverts);
    if (offset == -1) goto error;

    for (i = 0; i < npaths; i++) {
        GLNVGpath* copy = &rc->paths[call->pathOffset + i];
        const NVGpath* path = &paths[i];
        memset(copy, 0, sizeof(GLNVGpath));
        if (path->nstroke) {
            copy->strokeOffset = offset;
            copy->strokeCount = path->nstroke;
            memcpy(&rc->verts[offset], path->stroke, sizeof(NVGvertex) * path->nstroke);
            offset += path->nstroke;
        }
    }

    if (rc->flags & NVG_STENCIL_STROKES) {
        // Fill shader
        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(rc, 2);
        if (call->fragmentUniformBufferOffset == -1) goto error;

        glnvg__convertPaint(rc, nvg__fragUniformPtr(rc, call->fragmentUniformBufferOffset), paint, scissor, strokeWidth, fringe, -1.0f);
        glnvg__convertPaint(rc, nvg__fragUniformPtr(rc, call->fragmentUniformBufferOffset + rc->oneFragmentUniformBufferSize), paint, scissor, strokeWidth, fringe, 1.0f - 0.5f/255.0f);

    } else {
        // Fill shader
        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(rc, 1);
        if (call->fragmentUniformBufferOffset == -1) goto error;
        glnvg__convertPaint(rc, nvg__fragUniformPtr(rc, call->fragmentUniformBufferOffset), paint, scissor, strokeWidth, fringe, -1.0f);
    }

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (rc->ncalls > 0) rc->ncalls--;
}

static void glnvg__renderTriangles(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor,
                                   const NVGvertex* verts, int nverts, float fringe)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    GLNVGcall* call = glnvg__allocCall(rc);
    GLNVGfragUniforms* frag;

    if (call == NULL) return;

    call->type = GLNVG_TRIANGLES;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);

    // Allocate vertices for all the paths.
    call->triangleOffset = glnvg__allocVerts(rc, nverts);
    if (call->triangleOffset == -1) goto error;
    call->triangleCount = nverts;

    memcpy(&rc->verts[call->triangleOffset], verts, sizeof(NVGvertex) * nverts);

    // Fill shader
    call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(rc, 1);
    if (call->fragmentUniformBufferOffset == -1) goto error;
    frag = nvg__fragUniformPtr(rc, call->fragmentUniformBufferOffset);
    glnvg__convertPaint(rc, frag, paint, scissor, 1.0f, fringe, -1.0f);
    frag->type = NSVG_SHADER_IMG;

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (rc->ncalls > 0) rc->ncalls--;
}

static void glnvg__renderEndPrepare(void* uptr)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;

    if (rc->ncalls > 0) {
        ensureBufferCapacity(&rc->rhiVSUniformBuffer, 2 * sizeof(float));
        ensureBufferCapacity(&rc->rhiFSUniformBuffer, rc->nuniforms * rc->oneFragmentUniformBufferSize);
        ensureBufferCapacity(&rc->rhiVertexBuffer, rc->nverts * sizeof(NVGvertex));
        ensureBufferCapacity(&rc->rhiIndexBuffer, rc->nindices * sizeof(uint32_t));

        QRhiResourceUpdateBatch *u = resourceUpdateBatch(rc);
        u->updateDynamicBuffer(rc->rhiVSUniformBuffer, 0, 2 * sizeof(float), rc->view);
        u->updateDynamicBuffer(rc->rhiFSUniformBuffer, 0, rc->nuniforms * rc->oneFragmentUniformBufferSize, rc->uniforms);
        u->uploadStaticBuffer(rc->rhiVertexBuffer, 0, rc->nverts * sizeof(NVGvertex), rc->verts);
        if (rc->nindices)
            u->uploadStaticBuffer(rc->rhiIndexBuffer, 0, rc->nindices * sizeof(uint32_t), rc->indices);

        commitResourceUpdates(rc);

        const int dummyTexId = findTexture(rc, rc->dummyTex)->id;
        QRhiShaderResourceBindings *&srbWithDummyTexture(rc->srbs[dummyTexId]);
        if (!srbWithDummyTexture)
            srbWithDummyTexture = createSrb(rc, dummyTexId);

        for (int i = 0; i < rc->ncalls; i++) {
            GLNVGcall *call = &rc->calls[i];
            QRhiRenderPassDescriptor *rpDesc = rc->rt->renderPassDescriptor();

            QRhiShaderResourceBindings *&srbWithCallTexture(rc->srbs[call->image]);
            if (!srbWithCallTexture)
                srbWithCallTexture = createSrb(rc, call->image);

            PipelineState basePs;
            basePs.edgeAA = (rc->flags & NVG_ANTIALIAS) != 0;
            basePs.targetBlend.srcColor = call->blendFunc.srcRGB;
            basePs.targetBlend.dstColor = call->blendFunc.dstRGB;
            basePs.targetBlend.srcAlpha = call->blendFunc.srcAlpha;
            basePs.targetBlend.dstAlpha = call->blendFunc.dstAlpha;

            if (call->type == GLNVG_FILL) {
                call->srb[0] = srbWithDummyTexture;
                call->srb[1] = srbWithCallTexture;

                // 1. Draw shapes
                PipelineState ps = basePs;
                ps.stencilTestEnable = true;
                ps.stencilWriteMask = 0xFF;
                ps.stencilReadMask = 0xFF;
                ps.stencilFront = {
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::IncrementAndWrap,
                    QRhiGraphicsPipeline::Always
                };
                ps.stencilBack = {
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::DecrementAndWrap,
                    QRhiGraphicsPipeline::Always
                };
                ps.cullMode = QRhiGraphicsPipeline::None;
                ps.targetBlend.colorWrite = {};

                call->ps[0] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[0]), rpDesc, call->srb[0]);

                // 2. Draw anti-aliased pixels
                ps.cullMode = QRhiGraphicsPipeline::Back;
                ps.targetBlend.colorWrite = QRhiGraphicsPipeline::ColorMask(0xF);
                ps.stencilFront = {
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Equal
                };
                ps.stencilBack = ps.stencilFront;

                ps.topology = QRhiGraphicsPipeline::TriangleStrip;

                call->ps[1] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[1]), rpDesc, call->srb[1]);

                // 3. Draw fill
                ps.stencilFront = {
                    QRhiGraphicsPipeline::StencilZero,
                    QRhiGraphicsPipeline::StencilZero,
                    QRhiGraphicsPipeline::StencilZero,
                    QRhiGraphicsPipeline::NotEqual
                };
                ps.stencilBack = ps.stencilFront;

                call->ps[2] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[1]), rpDesc, call->srb[1]);
            } else if (call->type == GLNVG_CONVEXFILL) {
                call->srb[0] = srbWithCallTexture;

                // 1.
                call->ps[0] = pipeline(rc, PipelineStateKey::create(basePs, rpDesc, call->srb[0]), rpDesc, call->srb[0]);

                // 2. Draw fringes
                PipelineState ps = basePs;
                ps.topology = QRhiGraphicsPipeline::TriangleStrip;
                call->ps[1] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[0]), rpDesc, call->srb[0]);
            } else if (call->type == GLNVG_STROKE) {
                call->srb[0] = srbWithCallTexture;

                // 1. Draw Strokes (no stencil)
                PipelineState ps = basePs;
                ps.topology = QRhiGraphicsPipeline::TriangleStrip;
                call->ps[0] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[0]), rpDesc, call->srb[0]);

                // 2. Fill the stroke base without overlap
                ps.stencilTestEnable = true;
                ps.stencilWriteMask = 0xFF;
                ps.stencilReadMask = 0xFF;
                ps.stencilFront = {
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::IncrementAndClamp,
                    QRhiGraphicsPipeline::Equal
                };
                ps.stencilBack = ps.stencilFront;

                call->ps[1] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[0]), rpDesc, call->srb[0]);

                // 3. Draw anti-aliased pixels.
                ps.stencilFront = {
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Keep,
                    QRhiGraphicsPipeline::Equal
                };
                ps.stencilBack = ps.stencilFront;

                call->ps[2] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[0]), rpDesc, call->srb[0]);

                // 4. Clear stencil buffer
                ps.targetBlend.colorWrite = {};
                ps.stencilFront = {
                    QRhiGraphicsPipeline::StencilZero,
                    QRhiGraphicsPipeline::StencilZero,
                    QRhiGraphicsPipeline::StencilZero,
                    QRhiGraphicsPipeline::Always
                };
                ps.stencilBack = ps.stencilFront;

                call->ps[3] = pipeline(rc, PipelineStateKey::create(ps, rpDesc, call->srb[0]), rpDesc, call->srb[0]);
            } else if (call->type == GLNVG_TRIANGLES) {
                call->srb[0] = srbWithCallTexture;

                // 1.
                call->ps[0] = pipeline(rc, PipelineStateKey::create(basePs, rpDesc, call->srb[0]), rpDesc, call->srb[0]);
            }
        }



        // Upload ubo for frag shaders
        QGL->glBindBuffer(GL_UNIFORM_BUFFER, rc->fragmentUniformBuffer);
        QGL->glBufferData(GL_UNIFORM_BUFFER, rc->nuniforms * rc->oneFragmentUniformBufferSize, rc->uniforms, GL_STREAM_DRAW);

        // For vertex
        QGL->glBindBuffer(GL_UNIFORM_BUFFER, rc->vertexUniformBuffer);
        QGL->glBufferData(GL_UNIFORM_BUFFER, 2 * sizeof(float), rc->view, GL_STREAM_DRAW);

        // Upload vertex data
        QGL->glBindBuffer(GL_ARRAY_BUFFER, rc->vertBuf);
        QGL->glBufferData(GL_ARRAY_BUFFER, rc->nverts * sizeof(NVGvertex), rc->verts, GL_STREAM_DRAW);

        // Generate index data for triangle fan emulation
        if (rc->nindices) {
            QGL->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, rc->indexBuffer);
            QGL->glBufferData(GL_ELEMENT_ARRAY_BUFFER, rc->nindices * sizeof(uint32_t), rc->indices, GL_STREAM_DRAW);
        }
    }

    qDebug() << rc->pipelines.count() << rc->srbs.count();

}



// ********** render phase stuff (that is called after the command buffer has started to record a render pass)

static void renderpass_setUniforms(RhiNvgContext *rc, int fragmentUniformBufferOffset, int image)
{
    GLNVGtexture* tex = NULL;
    QGL->glBindBufferRange(GL_UNIFORM_BUFFER, 0, rc->vertexUniformBuffer, 0, 2 * sizeof(float));
    QGL->glBindBufferRange(GL_UNIFORM_BUFFER, 1, rc->fragmentUniformBuffer, fragmentUniformBufferOffset, sizeof(GLNVGfragUniforms));

    if (image != 0) {
        tex = findTexture(rc, image);
    }
    // If no image is set, use empty texture
    if (tex == NULL) {
        tex = findTexture(rc, rc->dummyTex);
    }
    QGL->glBindTexture(GL_TEXTURE_2D, tex != NULL ? tex->tex : 0);
}

static void renderpass_fill(RhiNvgContext *rc, GLNVGcall* call)
{
    GLNVGpath* paths = &rc->paths[call->pathOffset];
    int i, npaths = call->pathCount;

    // Draw shapes
    QGL->glEnable(GL_STENCIL_TEST);
    QGL->glStencilMask(0xff);
    QGL->glStencilFunc(GL_ALWAYS, 0, 0xff);
    QGL->glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);

    QGL->glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR_WRAP);
    QGL->glStencilOpSeparate(GL_BACK, GL_KEEP, GL_KEEP, GL_DECR_WRAP);
    QGL->glDisable(GL_CULL_FACE);

    // set bindpoint for solid loc
    renderpass_setUniforms(rc, call->fragmentUniformBufferOffset, 0);

    if (call->indexCount)
        QGL->glDrawElements(GL_TRIANGLES, call->indexCount, GL_UNSIGNED_INT, reinterpret_cast<const void *>(call->indexOffset * sizeof(quint32)));

    QGL->glEnable(GL_CULL_FACE);

    // Draw anti-aliased pixels
    QGL->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

    renderpass_setUniforms(rc, call->fragmentUniformBufferOffset + rc->oneFragmentUniformBufferSize, call->image);

    if (rc->flags & NVG_ANTIALIAS) {
        QGL->glStencilFunc(GL_EQUAL, 0x00, 0xff);
        QGL->glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
        // Draw fringes
        for (i = 0; i < npaths; i++)
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
    }

    // Draw fill
    QGL->glStencilFunc(GL_NOTEQUAL, 0x0, 0xff);
    QGL->glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);

    QGL->glDrawArrays(GL_TRIANGLE_STRIP, call->triangleOffset, call->triangleCount);

    QGL->glDisable(GL_STENCIL_TEST);
}

static void renderpass_convexFill(RhiNvgContext *rc, GLNVGcall* call)
{
    GLNVGpath* paths = &rc->paths[call->pathOffset];
    int i, npaths = call->pathCount;

    renderpass_setUniforms(rc, call->fragmentUniformBufferOffset, call->image);

    if (call->indexCount)
        QGL->glDrawElements(GL_TRIANGLES, call->indexCount, GL_UNSIGNED_INT, reinterpret_cast<const void *>(call->indexOffset * sizeof(quint32)));

    // Draw fringes
    for (i = 0; i < npaths; i++) {
        if (paths[i].strokeCount > 0) {
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
        }
    }
}

static void renderpass_stroke(RhiNvgContext *rc, GLNVGcall* call)
{
    GLNVGpath* paths = &rc->paths[call->pathOffset];
    int npaths = call->pathCount, i;

    if (rc->flags & NVG_STENCIL_STROKES) {

        QGL->glEnable(GL_STENCIL_TEST);
        QGL->glStencilMask(0xff);

        // Fill the stroke base without overlap
        QGL->glStencilFunc(GL_EQUAL, 0x0, 0xff);
        QGL->glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);

        renderpass_setUniforms(rc, call->fragmentUniformBufferOffset + rc->oneFragmentUniformBufferSize, call->image);

        for (i = 0; i < npaths; i++)
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);

        // Draw anti-aliased pixels.
        renderpass_setUniforms(rc, call->fragmentUniformBufferOffset, call->image);

        QGL->glStencilFunc(GL_EQUAL, 0x00, 0xff);
        QGL->glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);

        for (i = 0; i < npaths; i++)
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);

        // Clear stencil buffer.
        QGL->glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
        QGL->glStencilFunc(GL_ALWAYS, 0x0, 0xff);
        QGL->glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);

        for (i = 0; i < npaths; i++)
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);

        QGL->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

        QGL->glDisable(GL_STENCIL_TEST);

    } else {
        renderpass_setUniforms(rc, call->fragmentUniformBufferOffset, call->image);

        // Draw Strokes
        for (i = 0; i < npaths; i++)
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
    }
}

static void renderpass_triangles(RhiNvgContext *rc, GLNVGcall* call)
{
    renderpass_setUniforms(rc, call->fragmentUniformBufferOffset, call->image);
    QGL->glDrawArrays(GL_TRIANGLES, call->triangleOffset, call->triangleCount);
}

static void renderpass_render(void* uptr)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;

    QGL->glUseProgram(rc->prog);
    QGL->glEnable(GL_CULL_FACE);
    QGL->glCullFace(GL_BACK);
    QGL->glFrontFace(GL_CCW);
    QGL->glEnable(GL_BLEND);
    QGL->glDisable(GL_DEPTH_TEST);
    QGL->glDisable(GL_SCISSOR_TEST);
    QGL->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    QGL->glStencilMask(0xffffffff);
    QGL->glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    QGL->glStencilFunc(GL_ALWAYS, 0, 0xffffffff);

    QGL->glBindBuffer(GL_ARRAY_BUFFER, rc->vertBuf);
    QGL->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, rc->indexBuffer);
    QGL->glEnableVertexAttribArray(0);
    QGL->glEnableVertexAttribArray(1);
    QGL->glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(NVGvertex), (const GLvoid*)(size_t)0);
    QGL->glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(NVGvertex), (const GLvoid*)(0 + 2*sizeof(float)));

    QGL->glUniform1i(rc->texLoc, 0);

    for (int i = 0; i < rc->ncalls; i++) {
        GLNVGcall* call = &rc->calls[i];
        // no mapping to GL anymore, the standard premul alpha blend works because of the test app setting it on the context earlier
        //QGL->glBlendFuncSeparate(call->blendFunc.srcRGB, call->blendFunc.dstRGB, call->blendFunc.srcAlpha, call->blendFunc.dstAlpha);
        if (call->type == GLNVG_FILL)
            renderpass_fill(rc, call);
        else if (call->type == GLNVG_CONVEXFILL)
            renderpass_convexFill(rc, call);
        else if (call->type == GLNVG_STROKE)
            renderpass_stroke(rc, call);
        else if (call->type == GLNVG_TRIANGLES)
            renderpass_triangles(rc, call);
    }

    // Reset calls
    rc->nverts = 0;
    rc->npaths = 0;
    rc->ncalls = 0;
    rc->nuniforms = 0;
}

// ********** end of render pass stuff



static void glnvg__renderDelete(void* uptr)
{
    RhiNvgContext *rc = (RhiNvgContext*)uptr;
    int i;
    if (rc == NULL) return;

    if (rc->prog != 0)
        QGL->glDeleteProgram(rc->prog);

    if (rc->fragmentUniformBuffer != 0)
        QGL->glDeleteBuffers(1, &rc->fragmentUniformBuffer);

    if (rc->vertexUniformBuffer != 0)
        QGL->glDeleteBuffers(1, &rc->vertexUniformBuffer);

    if (rc->vertBuf != 0)
        QGL->glDeleteBuffers(1, &rc->vertBuf);

    if (rc->indexBuffer != 0)
        QGL->glDeleteBuffers(1, &rc->indexBuffer);

    for (i = 0; i < rc->ntextures; i++) {
        if (rc->textures[i].tex != 0 && (rc->textures[i].flags & NVG_IMAGE_NODELETE) == 0)
            QGL->glDeleteTextures(1, &rc->textures[i].tex);
        if (rc->textures[i].rhiTex && (rc->textures[i].flags & NVG_IMAGE_NODELETE) == 0)
            delete rc->textures[i].rhiTex;
    }
    free(rc->textures);

    for (const auto &samplerInfo : std::as_const(rc->samplers))
        delete samplerInfo.second;

    qDeleteAll(rc->pipelines);

    qDeleteAll(rc->srbs);

    delete rc->rhiVertexBuffer;
    delete rc->rhiIndexBuffer;
    delete rc->rhiVSUniformBuffer;
    delete rc->rhiFSUniformBuffer;

    free(rc->paths);
    free(rc->verts);
    free(rc->indices);
    free(rc->uniforms);
    free(rc->calls);

    delete rc;
}

NVGcontext* nvgCreateRhi(QRhi *rhi, int flags)
{
    NVGparams params;
    NVGcontext* ctx = NULL;
    RhiNvgContext *rc = new RhiNvgContext;
    if (rc == NULL) goto error;

    memset(&params, 0, sizeof(params));
    params.renderCreate = glnvg__renderCreate;
    params.renderCreateTexture = glnvg__renderCreateTexture;
    params.renderDeleteTexture = glnvg__renderDeleteTexture;
    params.renderUpdateTexture = glnvg__renderUpdateTexture;
    params.renderGetTextureSize = glnvg__renderGetTextureSize;
    params.renderViewport = glnvg__renderViewport;
    params.renderFill = glnvg__renderFill;
    params.renderStroke = glnvg__renderStroke;
    params.renderTriangles = glnvg__renderTriangles;
    params.renderDelete = glnvg__renderDelete;
    params.renderEndPrepare = glnvg__renderEndPrepare;
    params.renderRender = renderpass_render;
    params.userPtr = rc;
    params.edgeAntiAlias = flags & NVG_ANTIALIAS ? 1 : 0;

    rc->rhi = rhi;
    rc->flags = flags;

    ctx = nvgCreateInternal(&params);
    if (ctx == NULL) goto error;

    return ctx;

error:
    // 'gl' is freed by nvgDeleteInternal.
    if (ctx != NULL) nvgDeleteInternal(ctx);
    return NULL;
}

void nvgDeleteRhi(NVGcontext* ctx)
{
    nvgDeleteInternal(ctx);
}

void nvgBeginRhi(NVGcontext *ctx, QRhiCommandBuffer *cb, QRhiRenderTarget *rt)
{
    RhiNvgContext *rc = (RhiNvgContext*) nvgInternalParams(ctx)->userPtr;
    rc->cb = cb;
    rc->rt = rt;
    const QSize outputPixelSize = rt->pixelSize();
    nvgBegin(ctx, outputPixelSize.width(), outputPixelSize.height(), rt->devicePixelRatio());
}

QT_END_NAMESPACE

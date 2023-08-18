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
    int width, height;
    int type;
    int flags;
};
typedef struct GLNVGtexture GLNVGtexture;

struct GLNVGblend
{
    GLenum srcRGB;
    GLenum dstRGB;
    GLenum srcAlpha;
    GLenum dstAlpha;
};
typedef struct GLNVGblend GLNVGblend;

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
    int triangleOffset;
    int triangleCount;
    int fragmentUniformBufferOffset;
    int fragmentUniformBufferIndex;
    GLNVGblend blendFunc;
};
typedef struct GLNVGcall GLNVGcall;

struct GLNVGpath {
    int fillOffset;
    int fillCount;
    int strokeOffset;
    int strokeCount;
};
typedef struct GLNVGpath GLNVGpath;

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
typedef struct GLNVGfragUniforms GLNVGfragUniforms;

struct GLNVGcontext {
    QRhi *rhi;
    QRhiCommandBuffer *cb;
    QRhiRenderTarget *rt;

    GLuint prog;
    GLuint frag;
    GLuint vert;
    GLint texLoc;

    GLNVGtexture* textures;
    float view[2];
    int ntextures;
    int ctextures;
    int textureId;
    GLuint vertBuf;
    GLuint fragmentUniformBuffer;
    GLuint vertexUniformBuffer;
    int oneFragmentUniformBufferSize;
    int flags;

    // Per frame buffers
    GLNVGcall* calls;
    int ccalls;
    int ncalls;
    GLNVGpath* paths;
    int cpaths;
    int npaths;
    struct NVGvertex* verts;
    int cverts;
    int nverts;
    unsigned char* uniforms;
    int cuniforms;
    int nuniforms;

    int dummyTex;
};
typedef struct GLNVGcontext GLNVGcontext;

#define QGL QOpenGLContext::currentContext()->extraFunctions()

static GLNVGtexture* glnvg__allocTexture(GLNVGcontext* gl)
{
    GLNVGtexture* tex = NULL;
    int i;

    for (i = 0; i < gl->ntextures; i++) {
        if (gl->textures[i].id == 0) {
            tex = &gl->textures[i];
            break;
        }
    }
    if (tex == NULL) {
        if (gl->ntextures+1 > gl->ctextures) {
            GLNVGtexture* textures;
            int ctextures = std::max(gl->ntextures+1, 4) +  gl->ctextures/2; // 1.5x Overallocate
            textures = (GLNVGtexture*)realloc(gl->textures, sizeof(GLNVGtexture)*ctextures);
            if (textures == NULL) return NULL;
            gl->textures = textures;
            gl->ctextures = ctextures;
        }
        tex = &gl->textures[gl->ntextures++];
    }

    memset(tex, 0, sizeof(*tex));
    tex->id = ++gl->textureId;

    return tex;
}

static GLNVGtexture* glnvg__findTexture(GLNVGcontext* gl, int id)
{
    int i;
    for (i = 0; i < gl->ntextures; i++)
        if (gl->textures[i].id == id)
            return &gl->textures[i];
    return NULL;
}

static int glnvg__deleteTexture(GLNVGcontext* gl, int id)
{
    int i;
    for (i = 0; i < gl->ntextures; i++) {
        if (gl->textures[i].id == id) {
            if (gl->textures[i].tex != 0 && (gl->textures[i].flags & NVG_IMAGE_NODELETE) == 0)
                QGL->glDeleteTextures(1, &gl->textures[i].tex);
            memset(&gl->textures[i], 0, sizeof(gl->textures[i]));
            return 1;
        }
    }
    return 0;
}

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
    GLNVGcontext* gl = (GLNVGcontext*)uptr;

    if (gl->flags & NVG_ANTIALIAS) {
        if (glnvg__createShader(&gl->prog, fillVertShader, fillFragShaderAA) == 0)
            return 0;
    } else {
        if (glnvg__createShader(&gl->prog, fillVertShader, fillFragShader) == 0)
            return 0;
    }

    gl->texLoc = QGL->glGetUniformLocation(gl->prog, "tex");

    QGL->glGenBuffers(1, &gl->vertBuf);

    // Create UBOs
    QGL->glGenBuffers(1, &gl->fragmentUniformBuffer);
    QGL->glGenBuffers(1, &gl->vertexUniformBuffer);

    int ubufAlign = 4;
    QGL->glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, &ubufAlign);
    gl->oneFragmentUniformBufferSize = sizeof(GLNVGfragUniforms) + ubufAlign - sizeof(GLNVGfragUniforms) % ubufAlign;

    // Some platforms does not allow to have samples to unset textures.
    // Create empty one which is bound when there's no texture specified.
    gl->dummyTex = glnvg__renderCreateTexture(gl, NVG_TEXTURE_ALPHA, 1, 1, 0, NULL);

    return 1;
}

static int glnvg__renderCreateTexture(void* uptr, int type, int w, int h, int imageFlags, const unsigned char* data)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGtexture* tex = glnvg__allocTexture(gl);

    if (tex == NULL) return 0;

    QGL->glGenTextures(1, &tex->tex);
    tex->width = w;
    tex->height = h;
    tex->type = type;
    tex->flags = imageFlags;

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
    }

    return tex->id;
}


static int glnvg__renderDeleteTexture(void* uptr, int image)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    return glnvg__deleteTexture(gl, image);
}

static int glnvg__renderUpdateTexture(void* uptr, int image, int x, int y, int w, int h, const unsigned char* data)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGtexture* tex = glnvg__findTexture(gl, image);

    if (tex == NULL) return 0;

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
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGtexture* tex = glnvg__findTexture(gl, image);
    if (tex == NULL) return 0;
    *w = tex->width;
    *h = tex->height;
    return 1;
}

static void glnvg__xformToMat3x4(float* m3, float* t)
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

static NVGcolor glnvg__premulColor(NVGcolor c)
{
    c.r *= c.a;
    c.g *= c.a;
    c.b *= c.a;
    return c;
}

static int glnvg__convertPaint(GLNVGcontext* gl, GLNVGfragUniforms* frag, NVGpaint* paint,
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
        tex = glnvg__findTexture(gl, paint->image);
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

static GLNVGfragUniforms* nvg__fragUniformPtr(GLNVGcontext* gl, int i);

static void glnvg__setUniforms(GLNVGcontext* gl, int fragmentUniformBufferOffset, int image)
{
    GLNVGtexture* tex = NULL;
    QGL->glBindBufferRange(GL_UNIFORM_BUFFER, 0, gl->vertexUniformBuffer, 0, 2 * sizeof(float));
    QGL->glBindBufferRange(GL_UNIFORM_BUFFER, 1, gl->fragmentUniformBuffer, fragmentUniformBufferOffset, sizeof(GLNVGfragUniforms));

    if (image != 0) {
        tex = glnvg__findTexture(gl, image);
    }
    // If no image is set, use empty texture
    if (tex == NULL) {
        tex = glnvg__findTexture(gl, gl->dummyTex);
    }
    QGL->glBindTexture(GL_TEXTURE_2D, tex != NULL ? tex->tex : 0);
}

static void glnvg__renderViewport(void* uptr, float width, float height, float devicePixelRatio)
{
    NVG_NOTUSED(devicePixelRatio);
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    gl->view[0] = width;
    gl->view[1] = height;
}

static void glnvg__fill(GLNVGcontext* gl, GLNVGcall* call)
{
    GLNVGpath* paths = &gl->paths[call->pathOffset];
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
    glnvg__setUniforms(gl, call->fragmentUniformBufferOffset, 0);

    for (i = 0; i < npaths; i++)
        QGL->glDrawArrays(GL_TRIANGLE_FAN, paths[i].fillOffset, paths[i].fillCount);

    QGL->glEnable(GL_CULL_FACE);

    // Draw anti-aliased pixels
    QGL->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

    glnvg__setUniforms(gl, call->fragmentUniformBufferOffset + gl->oneFragmentUniformBufferSize, call->image);

    if (gl->flags & NVG_ANTIALIAS) {
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

static void glnvg__convexFill(GLNVGcontext* gl, GLNVGcall* call)
{
    GLNVGpath* paths = &gl->paths[call->pathOffset];
    int i, npaths = call->pathCount;

    glnvg__setUniforms(gl, call->fragmentUniformBufferOffset, call->image);

    for (i = 0; i < npaths; i++) {
        QGL->glDrawArrays(GL_TRIANGLE_FAN, paths[i].fillOffset, paths[i].fillCount);
        // Draw fringes
        if (paths[i].strokeCount > 0) {
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
        }
    }
}

static void glnvg__stroke(GLNVGcontext* gl, GLNVGcall* call)
{
    GLNVGpath* paths = &gl->paths[call->pathOffset];
    int npaths = call->pathCount, i;

    if (gl->flags & NVG_STENCIL_STROKES) {

        QGL->glEnable(GL_STENCIL_TEST);
        QGL->glStencilMask(0xff);

        // Fill the stroke base without overlap
        QGL->glStencilFunc(GL_EQUAL, 0x0, 0xff);
        QGL->glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);

        glnvg__setUniforms(gl, call->fragmentUniformBufferOffset + gl->oneFragmentUniformBufferSize, call->image);

        for (i = 0; i < npaths; i++)
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);

        // Draw anti-aliased pixels.
        glnvg__setUniforms(gl, call->fragmentUniformBufferOffset, call->image);

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
        glnvg__setUniforms(gl, call->fragmentUniformBufferOffset, call->image);

        // Draw Strokes
        for (i = 0; i < npaths; i++)
            QGL->glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
    }
}

static void glnvg__triangles(GLNVGcontext* gl, GLNVGcall* call)
{
    glnvg__setUniforms(gl, call->fragmentUniformBufferOffset, call->image);
    QGL->glDrawArrays(GL_TRIANGLES, call->triangleOffset, call->triangleCount);
}

static GLenum glnvg_convertBlendFuncFactor(int factor)
{
    if (factor == NVG_ZERO)
        return GL_ZERO;
    if (factor == NVG_ONE)
        return GL_ONE;
    if (factor == NVG_SRC_COLOR)
        return GL_SRC_COLOR;
    if (factor == NVG_ONE_MINUS_SRC_COLOR)
        return GL_ONE_MINUS_SRC_COLOR;
    if (factor == NVG_DST_COLOR)
        return GL_DST_COLOR;
    if (factor == NVG_ONE_MINUS_DST_COLOR)
        return GL_ONE_MINUS_DST_COLOR;
    if (factor == NVG_SRC_ALPHA)
        return GL_SRC_ALPHA;
    if (factor == NVG_ONE_MINUS_SRC_ALPHA)
        return GL_ONE_MINUS_SRC_ALPHA;
    if (factor == NVG_DST_ALPHA)
        return GL_DST_ALPHA;
    if (factor == NVG_ONE_MINUS_DST_ALPHA)
        return GL_ONE_MINUS_DST_ALPHA;
    if (factor == NVG_SRC_ALPHA_SATURATE)
        return GL_SRC_ALPHA_SATURATE;
    return GL_INVALID_ENUM;
}

static GLNVGblend glnvg__blendCompositeOperation(NVGcompositeOperationState op)
{
    GLNVGblend blend;
    blend.srcRGB = glnvg_convertBlendFuncFactor(op.srcRGB);
    blend.dstRGB = glnvg_convertBlendFuncFactor(op.dstRGB);
    blend.srcAlpha = glnvg_convertBlendFuncFactor(op.srcAlpha);
    blend.dstAlpha = glnvg_convertBlendFuncFactor(op.dstAlpha);
    if (blend.srcRGB == GL_INVALID_ENUM || blend.dstRGB == GL_INVALID_ENUM || blend.srcAlpha == GL_INVALID_ENUM || blend.dstAlpha == GL_INVALID_ENUM)
    {
        blend.srcRGB = GL_ONE;
        blend.dstRGB = GL_ONE_MINUS_SRC_ALPHA;
        blend.srcAlpha = GL_ONE;
        blend.dstAlpha = GL_ONE_MINUS_SRC_ALPHA;
    }
    return blend;
}

static void glnvg__renderEndPrepare(void* uptr)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;

    if (gl->ncalls > 0) {
        // Upload ubo for frag shaders
        QGL->glBindBuffer(GL_UNIFORM_BUFFER, gl->fragmentUniformBuffer);
        QGL->glBufferData(GL_UNIFORM_BUFFER, gl->nuniforms * gl->oneFragmentUniformBufferSize, gl->uniforms, GL_STREAM_DRAW);

        // For vertex
        QGL->glBindBuffer(GL_UNIFORM_BUFFER, gl->vertexUniformBuffer);
        QGL->glBufferData(GL_UNIFORM_BUFFER, 2 * sizeof(float), gl->view, GL_STREAM_DRAW);

        // Upload vertex data
        QGL->glBindBuffer(GL_ARRAY_BUFFER, gl->vertBuf);
        QGL->glBufferData(GL_ARRAY_BUFFER, gl->nverts * sizeof(NVGvertex), gl->verts, GL_STREAM_DRAW);
    }
}

static void glnvg__renderRender(void* uptr)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;

    QGL->glUseProgram(gl->prog);
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

    QGL->glBindBuffer(GL_ARRAY_BUFFER, gl->vertBuf);
    QGL->glEnableVertexAttribArray(0);
    QGL->glEnableVertexAttribArray(1);
    QGL->glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(NVGvertex), (const GLvoid*)(size_t)0);
    QGL->glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(NVGvertex), (const GLvoid*)(0 + 2*sizeof(float)));

    QGL->glUniform1i(gl->texLoc, 0);

    for (int i = 0; i < gl->ncalls; i++) {
        GLNVGcall* call = &gl->calls[i];
        QGL->glBlendFuncSeparate(call->blendFunc.srcRGB, call->blendFunc.dstRGB, call->blendFunc.srcAlpha, call->blendFunc.dstAlpha);
        if (call->type == GLNVG_FILL)
            glnvg__fill(gl, call);
        else if (call->type == GLNVG_CONVEXFILL)
            glnvg__convexFill(gl, call);
        else if (call->type == GLNVG_STROKE)
            glnvg__stroke(gl, call);
        else if (call->type == GLNVG_TRIANGLES)
            glnvg__triangles(gl, call);
    }

    // Reset calls
    gl->nverts = 0;
    gl->npaths = 0;
    gl->ncalls = 0;
    gl->nuniforms = 0;
}

static int glnvg__maxVertCount(const NVGpath* paths, int npaths)
{
    int i, count = 0;
    for (i = 0; i < npaths; i++) {
        count += paths[i].nfill;
        count += paths[i].nstroke;
    }
    return count;
}

static GLNVGcall* glnvg__allocCall(GLNVGcontext* gl)
{
    GLNVGcall* ret = NULL;
    if (gl->ncalls+1 > gl->ccalls) {
        GLNVGcall* calls;
        int ccalls = std::max(gl->ncalls+1, 128) + gl->ccalls/2; // 1.5x Overallocate
        calls = (GLNVGcall*)realloc(gl->calls, sizeof(GLNVGcall) * ccalls);
        if (calls == NULL) return NULL;
        gl->calls = calls;
        gl->ccalls = ccalls;
    }
    ret = &gl->calls[gl->ncalls++];
    memset(ret, 0, sizeof(GLNVGcall));
    return ret;
}

static int glnvg__allocPaths(GLNVGcontext* gl, int n)
{
    int ret = 0;
    if (gl->npaths+n > gl->cpaths) {
        GLNVGpath* paths;
        int cpaths = std::max(gl->npaths + n, 128) + gl->cpaths/2; // 1.5x Overallocate
        paths = (GLNVGpath*)realloc(gl->paths, sizeof(GLNVGpath) * cpaths);
        if (paths == NULL) return -1;
        gl->paths = paths;
        gl->cpaths = cpaths;
    }
    ret = gl->npaths;
    gl->npaths += n;
    return ret;
}

static int glnvg__allocVerts(GLNVGcontext* gl, int n)
{
    int ret = 0;
    if (gl->nverts+n > gl->cverts) {
        NVGvertex* verts;
        int cverts = std::max(gl->nverts + n, 4096) + gl->cverts/2; // 1.5x Overallocate
        verts = (NVGvertex*)realloc(gl->verts, sizeof(NVGvertex) * cverts);
        if (verts == NULL) return -1;
        gl->verts = verts;
        gl->cverts = cverts;
    }
    ret = gl->nverts;
    gl->nverts += n;
    return ret;
}

static int glnvg__allocFragUniforms(GLNVGcontext* gl, int n)
{
    int ret = 0, structSize = gl->oneFragmentUniformBufferSize;
    if (gl->nuniforms+n > gl->cuniforms) {
        unsigned char* uniforms;
        int cuniforms = std::max(gl->nuniforms+n, 128) + gl->cuniforms/2; // 1.5x Overallocate
        uniforms = (unsigned char*)realloc(gl->uniforms, structSize * cuniforms);
        if (uniforms == NULL) return -1;
        gl->uniforms = uniforms;
        gl->cuniforms = cuniforms;
    }
    ret = gl->nuniforms * structSize;
    gl->nuniforms += n;
    return ret;
}

static GLNVGfragUniforms* nvg__fragUniformPtr(GLNVGcontext* gl, int i)
{
    return (GLNVGfragUniforms*)&gl->uniforms[i];
}

static void glnvg__vset(NVGvertex* vtx, float x, float y, float u, float v)
{
    vtx->x = x;
    vtx->y = y;
    vtx->u = u;
    vtx->v = v;
}

static void glnvg__renderFill(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor, float fringe,
                              const float* bounds, const NVGpath* paths, int npaths)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGcall* call = glnvg__allocCall(gl);
    NVGvertex* quad;
    GLNVGfragUniforms* frag;
    int i, maxverts, offset;

    if (call == NULL) return;

    call->type = GLNVG_FILL;
    call->triangleCount = 4;
    call->pathOffset = glnvg__allocPaths(gl, npaths);
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
    maxverts = glnvg__maxVertCount(paths, npaths) + call->triangleCount;
    offset = glnvg__allocVerts(gl, maxverts);
    if (offset == -1) goto error;

    for (i = 0; i < npaths; i++) {
        GLNVGpath* copy = &gl->paths[call->pathOffset + i];
        const NVGpath* path = &paths[i];
        memset(copy, 0, sizeof(GLNVGpath));
        if (path->nfill > 0) {
            copy->fillOffset = offset;
            copy->fillCount = path->nfill;
            memcpy(&gl->verts[offset], path->fill, sizeof(NVGvertex) * path->nfill);
            offset += path->nfill;
        }
        if (path->nstroke > 0) {
            copy->strokeOffset = offset;
            copy->strokeCount = path->nstroke;
            memcpy(&gl->verts[offset], path->stroke, sizeof(NVGvertex) * path->nstroke);
            offset += path->nstroke;
        }
    }

    // Setup uniforms for draw calls
    if (call->type == GLNVG_FILL) {
        // Quad
        call->triangleOffset = offset;
        quad = &gl->verts[call->triangleOffset];
        glnvg__vset(&quad[0], bounds[2], bounds[3], 0.5f, 1.0f);
        glnvg__vset(&quad[1], bounds[2], bounds[1], 0.5f, 1.0f);
        glnvg__vset(&quad[2], bounds[0], bounds[3], 0.5f, 1.0f);
        glnvg__vset(&quad[3], bounds[0], bounds[1], 0.5f, 1.0f);

        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(gl, 2);
        if (call->fragmentUniformBufferOffset == -1) goto error;
        // Simple shader for stencil
        frag = nvg__fragUniformPtr(gl, call->fragmentUniformBufferOffset);
        memset(frag, 0, sizeof(*frag));
        frag->strokeThr = -1.0f;
        frag->type = NSVG_SHADER_SIMPLE;
        // Fill shader
        glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->fragmentUniformBufferOffset + gl->oneFragmentUniformBufferSize), paint, scissor, fringe, fringe, -1.0f);
    } else {
        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(gl, 1);
        if (call->fragmentUniformBufferOffset == -1) goto error;
        // Fill shader
        glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->fragmentUniformBufferOffset), paint, scissor, fringe, fringe, -1.0f);
    }

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (gl->ncalls > 0) gl->ncalls--;
}

static void glnvg__renderStroke(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor, float fringe,
                                float strokeWidth, const NVGpath* paths, int npaths)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGcall* call = glnvg__allocCall(gl);
    int i, maxverts, offset;

    if (call == NULL) return;

    call->type = GLNVG_STROKE;
    call->pathOffset = glnvg__allocPaths(gl, npaths);
    if (call->pathOffset == -1) goto error;
    call->pathCount = npaths;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);

    // Allocate vertices for all the paths.
    maxverts = glnvg__maxVertCount(paths, npaths);
    offset = glnvg__allocVerts(gl, maxverts);
    if (offset == -1) goto error;

    for (i = 0; i < npaths; i++) {
        GLNVGpath* copy = &gl->paths[call->pathOffset + i];
        const NVGpath* path = &paths[i];
        memset(copy, 0, sizeof(GLNVGpath));
        if (path->nstroke) {
            copy->strokeOffset = offset;
            copy->strokeCount = path->nstroke;
            memcpy(&gl->verts[offset], path->stroke, sizeof(NVGvertex) * path->nstroke);
            offset += path->nstroke;
        }
    }

    if (gl->flags & NVG_STENCIL_STROKES) {
        // Fill shader
        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(gl, 2);
        if (call->fragmentUniformBufferOffset == -1) goto error;

        glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->fragmentUniformBufferOffset), paint, scissor, strokeWidth, fringe, -1.0f);
        glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->fragmentUniformBufferOffset + gl->oneFragmentUniformBufferSize), paint, scissor, strokeWidth, fringe, 1.0f - 0.5f/255.0f);

    } else {
        // Fill shader
        call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(gl, 1);
        if (call->fragmentUniformBufferOffset == -1) goto error;
        glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->fragmentUniformBufferOffset), paint, scissor, strokeWidth, fringe, -1.0f);
    }

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (gl->ncalls > 0) gl->ncalls--;
}

static void glnvg__renderTriangles(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor,
                                   const NVGvertex* verts, int nverts, float fringe)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGcall* call = glnvg__allocCall(gl);
    GLNVGfragUniforms* frag;

    if (call == NULL) return;

    call->type = GLNVG_TRIANGLES;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);

    // Allocate vertices for all the paths.
    call->triangleOffset = glnvg__allocVerts(gl, nverts);
    if (call->triangleOffset == -1) goto error;
    call->triangleCount = nverts;

    memcpy(&gl->verts[call->triangleOffset], verts, sizeof(NVGvertex) * nverts);

    // Fill shader
    call->fragmentUniformBufferOffset = glnvg__allocFragUniforms(gl, 1);
    if (call->fragmentUniformBufferOffset == -1) goto error;
    frag = nvg__fragUniformPtr(gl, call->fragmentUniformBufferOffset);
    glnvg__convertPaint(gl, frag, paint, scissor, 1.0f, fringe, -1.0f);
    frag->type = NSVG_SHADER_IMG;

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (gl->ncalls > 0) gl->ncalls--;
}

static void glnvg__renderDelete(void* uptr)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    int i;
    if (gl == NULL) return;

    if (gl->prog != 0)
        QGL->glDeleteProgram(gl->prog);

    if (gl->fragmentUniformBuffer != 0)
        QGL->glDeleteBuffers(1, &gl->fragmentUniformBuffer);

    if (gl->vertexUniformBuffer != 0)
        QGL->glDeleteBuffers(1, &gl->vertexUniformBuffer);

    if (gl->vertBuf != 0)
        QGL->glDeleteBuffers(1, &gl->vertBuf);

    for (i = 0; i < gl->ntextures; i++) {
        if (gl->textures[i].tex != 0 && (gl->textures[i].flags & NVG_IMAGE_NODELETE) == 0)
            QGL->glDeleteTextures(1, &gl->textures[i].tex);
    }
    free(gl->textures);

    free(gl->paths);
    free(gl->verts);
    free(gl->uniforms);
    free(gl->calls);

    free(gl);
}

NVGcontext* nvgCreateRhi(QRhi *rhi, int flags)
{
    NVGparams params;
    NVGcontext* ctx = NULL;
    GLNVGcontext* gl = (GLNVGcontext*)malloc(sizeof(GLNVGcontext));
    if (gl == NULL) goto error;
    memset(gl, 0, sizeof(GLNVGcontext));

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
    params.renderRender = glnvg__renderRender;
    params.userPtr = gl;
    params.edgeAntiAlias = flags & NVG_ANTIALIAS ? 1 : 0;

    gl->rhi = rhi;
    gl->flags = flags;

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
    GLNVGcontext* gl = (GLNVGcontext*) nvgInternalParams(ctx)->userPtr;
    gl->cb = cb;
    gl->rt = rt;
    const QSize outputPixelSize = rt->pixelSize();
    nvgBegin(ctx, outputPixelSize.width(), outputPixelSize.height(), rt->devicePixelRatio());
}

QT_END_NAMESPACE

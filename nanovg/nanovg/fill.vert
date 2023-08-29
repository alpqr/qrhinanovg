#version 440

layout(location = 0) in vec2 vertex;
layout(location = 1) in vec2 tcoord;
layout(location = 0) out vec2 ftcoord;
layout(location = 1) out vec2 fpos;

layout(std140, binding = 0) uniform vertUBuf {
    vec4 viewRect;
    int ndcIsYDown;
};

void main()
{
    ftcoord = tcoord;
    fpos = vertex;
    if (ndcIsYDown != 0)
        gl_Position = vec4(2.0 * (vertex.x + viewRect.x) / viewRect.z - 1.0, -1.0 + 2.0 * (vertex.y + viewRect.y) / viewRect.w, 0.0, 1.0);
    else
        gl_Position = vec4(2.0 * (vertex.x + viewRect.x) / viewRect.z - 1.0, 1.0 - 2.0 * (vertex.y + viewRect.y) / viewRect.w, 0.0, 1.0);
}

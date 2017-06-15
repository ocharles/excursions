#version 330

out vec2 v_texCoord;

void main()
{
    v_texCoord = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    gl_Position = vec4(v_texCoord * 2 - 1, 0.0f, 1.0f);
}
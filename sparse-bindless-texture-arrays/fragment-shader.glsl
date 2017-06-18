#version 430
#extension GL_ARB_bindless_texture : require
#extension GL_EXT_texture_array : require

in vec2 v_texCoord;

out vec3 fResult;

layout (bindless_sampler) uniform sampler2DArray t;

uniform TBlock
{
    float textureIndex;
} T;

void main() {
   fResult = texture2DArray(t, vec3(v_texCoord.xy, T.textureIndex)).rgb;
}

#version 330

in vec2 v_texCoord;

out vec3 fResult;

uniform sampler2D t;

void main() {
   fResult = texture(t, v_texCoord).rgb;
}

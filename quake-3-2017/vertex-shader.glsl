#version 330

out vec2 v_texCoord;
out vec2 v_texCoord_lm;

flat out uint drawId;

uniform mat4 u_projViewModel;

layout (location = 0) in vec3 a_position;
layout (location = 3) in vec3 a_normal;
layout (location = 1) in vec2 a_uv_0;
layout (location = 2) in vec2 a_uv_1;
layout (location = 4) in uint a_face_index;

vec3 swizzle(vec3 a) {
  return vec3(a.x, a.z, -a.y);
}

void main()
{
  vec3 position = swizzle(a_position);
  gl_Position = u_projViewModel * vec4(position, 1);
  drawId = a_face_index;

  v_texCoord = a_uv_0;
  v_texCoord_lm = a_uv_1;
}

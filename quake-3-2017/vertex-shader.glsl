#version 330

#extension GL_ARB_shader_draw_parameters : require

out vec2 v_texCoord;
out vec2 v_texCoord_lm;

flat out int drawId;

uniform mat4 u_projViewModel;

in vec3 a_position;
in vec3 a_normal;
in vec2 a_uv_0;
in vec2 a_uv_1;

vec3 swizzle(vec3 a) {
  return vec3(a.x, a.z, -a.y);
}

void main()
{
  vec3 position = swizzle(a_position);
  gl_Position = u_projViewModel * vec4(position, 1);
  drawId = gl_DrawIDARB;

  v_texCoord = a_uv_0;
  v_texCoord_lm = a_uv_1;
}

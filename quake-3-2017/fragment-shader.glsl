#version 430

#extension GL_ARB_bindless_texture : require


struct Pass {
  int sourceFactors[5];
  int destFactors[5];
  layout (bindless_sampler) sampler2D diffuseTexture;
};

struct Material {
  int nPasses;
  int firstPass;
};

struct DrawInfo {
  int materialIndex;
};

layout (binding = 0) buffer Materials {
  Material materials[];
};

layout (binding = 1) buffer Passes {
  Pass passes[];
};

layout (binding = 2) buffer DrawInfos {
  DrawInfo faceDrawInfos[];
};

flat in int drawId;

in vec2 v_texCoord;

out vec4 result;

void main() {

  DrawInfo di = faceDrawInfos[drawId];
  Material m = materials[di.materialIndex];

  result = vec4(0);

  for (int i = 0; i < m.nPasses; i++) {
    Pass p = passes[m.firstPass + i];
    vec4 source = texture2D(p.diffuseTexture, v_texCoord);
    result =
      source
        * (vec4(p.sourceFactors[0]) +
           vec4(p.sourceFactors[1]) * source +
           vec4(p.sourceFactors[2]) * result +
           vec4(p.sourceFactors[3]) * source.aaaa +
           vec4(p.sourceFactors[4]) * result.aaaa
           )
      +
      result
      * (vec4(p.destFactors[0]) +
         vec4(p.destFactors[1]) * source +
         vec4(p.destFactors[2]) * result +
         vec4(p.destFactors[3]) * source.aaaa +
         vec4(p.destFactors[4]) * result.aaaa
         );
  }
}

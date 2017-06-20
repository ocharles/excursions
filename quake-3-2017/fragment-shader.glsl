#version 430

struct Pass {
  int sourceFactors[5];
  int destFactors[5];
  layout (bindless) sampler2D diffuseTexture;
};

struct Material {
  int nPasses;
  int firstPass;
};

struct DrawInfo {
  int materialIndex;
  layout (bindless) sampler2D lightMapTexture;
};

buffer DrawInfos {
  DrawInfo faceDrawInfos[];
};

buffer Passes {
  Pass passes[];
}

buffer Materials {
  Material materials[];
}

in vec2 v_uv;

void main() {

  DrawInfo di = faceDrawInfos[gl_DrawID];
  Material m = materials[di.materialIndex];

  vec4 result = vec4(0);

  for (int i = 0; i < m.nPasses; i++) {
    Pass p = passes[m.firstPass + i];
    vec4 source = texture2D(p.diffuseTexture, v_uv);
    result =
      source * (vec4(p.sourceFactors[0]) + vec4(p.sourceFactors[1]) * source + vec4(p.sourceFactors[2]) * result)
      +
      result * (vec4(p.destFactors[0]) + vec4(p.destFactors[1]) * source + vec4(p.destFactors[2]) * result);
  }

  gl_FragColor = result;
}

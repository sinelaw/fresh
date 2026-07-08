#include <metal_stdlib>
using namespace metal;

struct VertexOut {
  float4 position [[position]];
  half4 color;
};

kernel void add_vectors(device const float *a [[buffer(0)]],
                        device const float *b [[buffer(1)]],
                        device float *out [[buffer(2)]],
                        uint id [[thread_position_in_grid]]) {
  out[id] = clamp(a[id] + b[id], 0.0f, 1.0f);
}

fragment half4 shade(VertexOut in [[stage_in]],
                     texture2d<float, access::sample> image [[texture(0)]],
                     sampler linear_sampler [[sampler(0)]]) {
  return half4(image.sample(linear_sampler, in.position.xy).rgb, 1.0h);
}

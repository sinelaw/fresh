// HLSL compute shader sample for syntax-highlighting coverage tests.
cbuffer CameraConstants : register(b0)
{
    float4x4 viewProjection;
    float3 cameraPosition;
    float  exposure;
};

Texture2D<float4> sourceTexture : register(t0);
RWTexture2D<float4> targetTexture : register(u0);
SamplerState linearSampler : register(s0);

[numthreads(8, 8, 1)]
void main(uint3 dispatchThreadId : SV_DispatchThreadID)
{
    uint2 pixel = dispatchThreadId.xy;
    float2 uv = (float2(pixel) + 0.5f) / 512.0f;
    float4 source = sourceTexture.SampleLevel(linearSampler, uv, 0.0f);
    float3 mapped = 1.0f - exp(-source.rgb * exposure);

    if (all(mapped >= 0.0f))
    {
        targetTexture[pixel] = float4(mapped, source.a);
    }
}

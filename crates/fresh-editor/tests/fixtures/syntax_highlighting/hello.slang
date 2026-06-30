// Slang compute shader sample for syntax-highlighting coverage tests.
import playground;

static const uint THREADS = 8;

struct Particle
{
    float3 position;
    float3 velocity;
    float  mass;
};

[shader("compute")]
[numthreads(THREADS, THREADS, 1)]
void computeMain(uint3 tid: SV_DispatchThreadID)
{
    RWStructuredBuffer<Particle> particles;
    float4x4 transform = float4x4(1.0);

    uint index = tid.x;
    Particle p = particles[index];

    float3 gravity = float3(0.0f, -9.81f, 0.0f);
    p.velocity += gravity * 0.016;
    p.position = mul(transform, float4(p.position, 1.0)).xyz;

    if (length(p.velocity) > 100.0)
    {
        p.velocity = normalize(p.velocity) * 100.0;
    }

    particles[index] = p;
}

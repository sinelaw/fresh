// WGSL compute shader sample for syntax-highlighting coverage tests.
struct Particle {
    position: vec4<f32>,
    velocity: vec4<f32>,
};

@group(0) @binding(0)
var<storage, read_write> particles: array<Particle>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let index = global_id.x;
    var particle = particles[index];
    let gravity = vec4<f32>(0.0, -9.81, 0.0, 0.0);

    particle.velocity = particle.velocity + gravity * 0.016;
    particle.position = particle.position + particle.velocity * 0.016;

    if (length(particle.velocity.xyz) > 100.0) {
        particle.velocity = normalize(particle.velocity) * 100.0;
    }

    particles[index] = particle;
}

#version 450

layout(local_size_x = 8, local_size_y = 8, local_size_z = 1) in;
layout(set = 0, binding = 0, rgba32f) uniform image2D targetImage;

const float exposure = 1.25;

vec3 tonemap(vec3 color)
{
    return vec3(1.0) - exp(-color * exposure);
}

void main()
{
    ivec2 pixel = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(pixel) / vec2(imageSize(targetImage));
    vec3 color = vec3(uv, 0.5);

    if (color.r > 0.25)
    {
        imageStore(targetImage, pixel, vec4(tonemap(color), 1.0));
    }
}

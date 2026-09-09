#version 450

layout(location = 0) out vec4 fragmentColor;
uniform sampler2D albedo;

void main()
{
    vec3 color = texture(albedo, vec2(0.5)).rgb;
    fragmentColor = vec4(color, 1.0);
}

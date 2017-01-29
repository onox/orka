#version 330 core

#extension GL_ARB_shader_subroutine : require

in  vec3 in_Position;
in  vec3 in_Color;
out vec3 ex_Color;

subroutine vec3 update_position(in vec3 position);
subroutine vec3 no_update_position(in vec3 position);

subroutine(update_position)
vec3 update_x(in vec3 position)
{
    position.x -= 0.4;
    position.y -= 0.4;
    return position;
}

subroutine(no_update_position)
vec3 update_z(in vec3 position)
{
    position.x += 0.0;
    position.y += 0.0;
    return position;
}

subroutine(update_position)
vec3 update_y(in vec3 position)
{
    position.x += 0.4;
    position.y += 0.4;
    return position;
}

subroutine uniform update_position[2] update_pos;
subroutine uniform no_update_position no_update_pos;

void main(void) {
   gl_Position = vec4(update_pos[0](in_Position), 1.0);
   ex_Color = in_Color;
}

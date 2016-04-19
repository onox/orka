#version 400 core

in  vec3 in_Position;
in  vec3 in_Color;
out vec3 ex_Color;

subroutine vec3 update_position(in vec3 position);

subroutine(update_position)
vec3 update_x(in vec3 position)
{
    position.x -= 0.4;
    position.y -= 0.4;
    return position;
}

subroutine(update_position)
vec3 update_y(in vec3 position)
{
    position.x += 0.4;
    position.y += 0.4;
    return position;
}

subroutine uniform update_position update_pos;

void main(void) {
   gl_Position = vec4(update_pos(in_Position), 1.0);
   ex_Color = in_Color;
}

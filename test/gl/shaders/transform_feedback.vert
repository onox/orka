#version 150 core

in  float in_value;
out float geom_value;

void main(void) {
   geom_value = sqrt(in_value);
}

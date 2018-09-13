#version 330 core

#extension GL_ARB_compute_shader : require
#extension GL_ARB_shader_storage_buffer_object : require

uniform uint maxNumbers;

layout(local_size_x = 1024) in;
// Must be a power of two

layout(std430, binding = 0) restrict buffer inputBuffer {
    uint numbers[];
};

// Parallel reduction
//
// References:
// [1] http://developer.download.nvidia.com/assets/cuda/files/reduction.pdf
// [2] https://diaryofagraphicsprogrammer.blogspot.com/2014/03/compute-shader-optimizations-for-amd.html

shared uint sdata[gl_WorkGroupSize.x];

void main(void) {
    uint gid = gl_GlobalInvocationID.x;
    uint tid = gl_LocalInvocationID.x;

    if (gid < maxNumbers) {
        sdata[tid] = numbers[gid];
    }
    else {
        sdata[tid] = 0u;
    }
    barrier();

    // Interleaved addressing with divergent branching (slide 8 of [1]):
    // Even threads (0, 2, 4, etc.) are active, odd threads
    // are idle.
    // Each thread also has bank conflicts because even threads
    // access the memory of the (idle) odd threads.
    /*for (uint s = 1u; s < gl_WorkGroupSize.x; s *= 2u) {
        if (tid % (2u * s) == 0u) {
            sdata[tid] += sdata[tid + s];
        }
        barrier();
    }*

    // Interleaved addressing with bank conflicts (slide 12 of [1]):
    // Only threads 0 to n / 2 - 1 are active, but memory access
    // is still interleaved: thread 0 accesses memory 0 and 1,
    // thread 1 accesses 2 and 3, etc.
    /*for (uint s = 1u; s < gl_WorkGroupSize.x; s *= 2u) {
        uint index = 2u * s * tid;
        if (index < gl_WorkGroupSize.x) {
            sdata[index] += sdata[index + s];
        }
        barrier();
    }*/

    // Sequential addressing (slide 14 of [1]):
    // For a group size with n threads, the left halve (n / 2 threads)
    // threads (0 .. n / 2 - 1) add the right halve (n / 2 .. n) (due to
    // stride s) to themselves.
    for (uint s = gl_WorkGroupSize.x / 2u; s > 0u; s >>= 1u) {
        if (tid < s) {
            sdata[tid] += sdata[tid + s];
        }
        barrier();
    }

    if (tid == 0u) {
        numbers[gl_WorkGroupID.x] = sdata[0];
    }
}

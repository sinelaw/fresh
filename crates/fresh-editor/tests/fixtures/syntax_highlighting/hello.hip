#include <hip/hip_runtime.h>

__global__ void fill(float *out, float value, int n) {
  int id = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
  if (id < n) {
    out[id] = value;
  }
}

int main() {
  float *device_out = nullptr;
  hipError_t status = hipMalloc(&device_out, 4096);
  if (status == hipSuccess) {
    dim3 grid(8);
    dim3 block(128);
    hipLaunchKernelGGL(fill, grid, block, 0, 0, device_out, 1.0f, 1024);
    hipDeviceSynchronize();
  }
  hipFree(device_out);
  return 0;
}

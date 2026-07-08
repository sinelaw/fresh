#include <cuda_runtime.h>

__global__ void saxpy(float a, const float *x, const float *y, float *out, int n) {
  int id = blockIdx.x * blockDim.x + threadIdx.x;
  if (id < n) {
    out[id] = fmaf(a, x[id], y[id]);
  }
}

int main() {
  float *device_out = nullptr;
  cudaError_t status = cudaMalloc(&device_out, 1024 * sizeof(float));
  if (status == cudaSuccess) {
    dim3 grid(4);
    dim3 block(256);
    saxpy<<<grid, block>>>(2.0f, device_out, device_out, device_out, 1024);
    cudaDeviceSynchronize();
  }
  cudaFree(device_out);
  return 0;
}

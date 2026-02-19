#include <stdio.h>
#include <stdlib.h>

/* Multiply C = A(m x k) * B(k x n), row-major storage. */
static void matmul(int m, int n, int k, const double *A, const double *B, double *C) {
  int i, j, t;
  for (i = 0; i < m; i++) {
    for (j = 0; j < n; j++) {
      double s = 0.0;
      for (t = 0; t < k; t++) {
        s += A[i * k + t] * B[t * n + j];
      }
      C[i * n + j] = s;
    }
  }
}

int main(void) {
  int m = 2, k = 3, n = 2;
  int i;
  double *A = (double*)malloc((size_t)(m * k) * sizeof(double));
  double *B = (double*)malloc((size_t)(k * n) * sizeof(double));
  double *C = (double*)malloc((size_t)(m * n) * sizeof(double));

  if (!A || !B || !C) {
    printf("alloc fail\n");
    free(A);
    free(B);
    free(C);
    return 1;
  }

  /* A = [1 2 3; 4 5 6], B = [7 8; 9 10; 11 12] */
  A[0] = 1.0; A[1] = 2.0; A[2] = 3.0;
  A[3] = 4.0; A[4] = 5.0; A[5] = 6.0;
  B[0] = 7.0; B[1] = 8.0;
  B[2] = 9.0; B[3] = 10.0;
  B[4] = 11.0; B[5] = 12.0;

  matmul(m, n, k, A, B, C);

  printf("C:\n");
  for (i = 0; i < m * n; i++) {
    printf(" %d", (int)C[i]);
  }
  printf("\n");

  free(A);
  free(B);
  free(C);
  return 0;
}

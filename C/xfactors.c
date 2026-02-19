#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* return the positive divisors ("factors") of n in ascending order.
   - n must be positive
   - out gets a malloc'd array that the caller must free
   - return value is the number of factors (0 on error)
*/
int factors(int n, int **out)
{
	int r, i;
	int cnt_small = 0, cnt_total = 0, k = 0;
	int *a;

	if (out == NULL) return 0;
	*out = NULL;

	if (n <= 0) return 0;

	r = (int)floor(sqrt((double)n));

	/* count small divisors (<= sqrt(n)) and total divisors */
	for (i = 1; i <= r; i++) {
		if (n % i == 0) {
			cnt_small++;
			if (i == n / i) cnt_total += 1;  /* perfect square */
			else            cnt_total += 2;
		}
	}

	a = (int *)malloc((size_t)cnt_total * sizeof(int));
	if (a == NULL) return 0;

	/* fill ascending:
	   1) small divisors increasing
	   2) corresponding large divisors decreasing (so overall increasing)
*/
	for (i = 1; i <= r; i++) {
		if (n % i == 0) a[k++] = i;
	}
	for (i = r; i >= 1; i--) {
		if (n % i == 0) {
			int j = n / i;
			if (j != i) a[k++] = j;
		}
	}

	*out = a;
	return cnt_total;
}

int main(void)
{
	int n;
	for (n = 1; n <= 10; n++) {
		int *f = NULL;
		int nf = factors(n, &f);

		printf("%d:", n);
		for (int i = 0; i < nf; i++) {
			printf(" %d", f[i]);
		}
		printf("\n");

		free(f);
	}
	return 0;
}

#include <stdio.h>
#include <math.h>

typedef int point_t;

int add (unsigned int a, point_t p) {
	int i = 0;
	unsigned int m = 0;
	int max = ceil(log2(a));
	point_t q = p;
	for (i=max-2;i>=0;i--) {
		q+=q;
		m=floor(pow(2,i));
		if (a & m) q+=p;
	}
	return q;
}

int main() {
	printf("%d\n", add(28,5));
}

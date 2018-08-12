/* ------------------------------------------------------------------------
 * bit tricks using 8-bit words
 * ------------------------------------------------------------------------
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

#define HBYTE 0x80
#define LBYTE 0x01

#define NLBYTE 0x0a

void negative() {
	fprintf(stdout, "%x %x %x\n", 5, -5, 5 & -5);

	for(uint32_t k=8; k>0; k>>=1) {
		fprintf(stdout, "%d ", (5 & k) != 0);
	}
	fprintf(stdout, "\n");
	fprintf(stdout, "-1: ");
	for(uint32_t k=8; k>0; k>>=1) {
		fprintf(stdout, "%d ", (-1 & k) != 0);
	}
	fprintf(stdout, "\n");
	fprintf(stdout, "-2: ");
	for(uint32_t k=8; k>0; k>>=1) {
		fprintf(stdout, "%d ", (-2 & k) != 0);
	}
	fprintf(stdout, "\n");
	fprintf(stdout, "-3: ");
	for(uint32_t k=8; k>0; k>>=1) {
		fprintf(stdout, "%d ", (-3 & k) != 0);
	}
	fprintf(stdout, "\n");
	fprintf(stdout, "-4: ");
	for(uint32_t k=8; k>0; k>>=1) {
		fprintf(stdout, "%d ", (-4 & k) != 0);
	}
	fprintf(stdout, "\n");
	fprintf(stdout, "-5: ");
	for(uint32_t k=8; k>0; k>>=1) {
		fprintf(stdout, "%d ", (-5 & k) != 0);
	}
	fprintf(stdout, "\n");
	fprintf(stdout, "-8: ");
	for(uint32_t k=8; k>0; k>>=1) {
		fprintf(stdout, "%d ", (-8 & k) != 0);
	}
	fprintf(stdout, "\n");
}

/* these mask are -1/(3),
 *                -1/(5),
 *                -1/(17), 
 *                 ...
 * i.e.: -1/(2^(2^k)+1) */
int mu(int k) {
	int z = 1 << k;
	int b = 1 << z;
	
	return (255/(b+1));
}

void bits(int x) {
	for(int k=8; k>0; k>>=1) {
		fprintf(stdout, "%d", (x&k)!=0);
	}
}

int rrm1(int x) {
	return (x & (x-1));
}

int erm1(int x) {
	return (x & (-x));
}

int srm1l(int x) {
	return (x | (-x));
}

int rsrm1l(int x) {
	return (x ^ (-x));
}

int off(int x) {
	return (x & 0xf);
}

int rho(int x) {
	int r=0;

	if (x == 0) return -1;
	for(int k=0; k<3; k++) {
		if ((x & mu(k)) == 0) r+=(1<<k);
	}
	return r;
}

int lm1(int x) {
	int l = 0;
	int y = x;

	if (x == 0) return -1;
	for(int k=2; k>=0; k--) {
		int z=y>>(1<<k);
		if (z != 0) {
			y=z; l+=(1<<k);
		}
	}
	return l;
}

int rm1(int x) {
	return (rho(erm1(x)));
}

int64_t popcount(int64_t x) {
	// int64_t a = 0xff;
	int64_t y = x - ((x >> 1) & mu(0));
	y = (y & mu(1)) + ((y >> 2) & mu(1));
	y = (y + (y >> 4)) & mu(2);
	/* we don't need this for 8bit numbers */
	// return (((a*y) % 0xffffffffffffffff) >> 56);
	return y;
}

int reverse(int x) {
	// swap bits
	int y = (x >> 1) & mu(0);
	int z = (x & mu(0)) << 1;
	int r = y | z;

	// swap nyps
	y = (r >> 2) & mu(1);
	z = (r & mu(1)) << 2;
	r = y | z;

	// swap nybbles
	/*
	y = (r >> 4) & mu(2);
	z = (r & mu(2)) << 4;
	r = y | z;
	*/

	return r;
}

int nextSubset(int u, int x) {
	return ((x-u)&u);
}

int neg(int x) {
	return (-x-1);
	// return (~x);
}

int isnull(int x) {
	return (HBYTE & (x-LBYTE) & (~x));
}

int main() {
	srand(time(NULL));
	// negative();

	fprintf(stdout, "remove rightmost 1\n");
	for(int i=0; i<10; i++) {
		int x = rand()%8;
		fprintf(stdout, "%d: ", x);
		bits(x);
		fprintf(stdout, " --> ");
		bits(rrm1(x));
 		fprintf(stdout, "\n");
	}

	fprintf(stdout, "extract rightmost 1\n");
	for(int i=0; i<10; i++) {
		int x = rand()%8;
		fprintf(stdout, "%d: ", x);
		bits(x);
		fprintf(stdout, " --> ");
		int y = erm1(x);
		bits(y);
		fprintf(stdout, " rho: %d\n", rho(y));
	}

	fprintf(stdout, "rightmost and leftmost 1\n");
	for(int i=0; i<10; i++) {
		int x = rand()%8;
		
		fprintf(stdout, "%d (", x);
		bits(x);
		fprintf(stdout, "): % 2d  | % 2d\n", rm1(x), lm1(x));
	}

	fprintf(stdout, "rightmost and leftmost 0\n");
	for(int i=0; i<10; i++) {
		int x = rand()%16;
		
		fprintf(stdout, "%02d (", x);
		bits(x);
		/* note that for lm0, we have to unset
		 * all bits beyone our "nibble" data type */
		fprintf(stdout, "): % 2d  | % 2d\n",
		             rm1(~x), lm1(off(~x)));
	}

	fprintf(stdout, "popcount\n");
	for(int i=0; i<16; i++) {
		int x = i; // rand()%16;
		
		fprintf(stdout, "%02d (", x);
		bits(x);
		fprintf(stdout, "): % 2ld\n", popcount((int64_t)x));
	}
	fprintf(stdout, "reverse\n");
	for(int i=0; i<16; i++) {
		int x = i; // rand()%16;
		
		fprintf(stdout, "%02d (", x);
		bits(x);
		int r = reverse(x);
		fprintf(stdout, "): % 2d (", r);
		bits(r);
		fprintf(stdout, ")\n");
	}

	fprintf(stdout, "smear rightmost 1 to the left\n");
	for(int i=0; i<10; i++) {
		int x = rand()%8;
		fprintf(stdout, "%d: ", x);
		bits(x);
		fprintf(stdout, " --> ");
		bits(srm1l(x));
 		fprintf(stdout, "\n");
	}

	fprintf(stdout, "remove and smear rightmost 1 to the left\n");
	for(int i=0; i<10; i++) {
		int x = rand()%8;
		fprintf(stdout, "%d: ", x);
		bits(x);
		fprintf(stdout, " --> ");
		bits(rsrm1l(x));
 		fprintf(stdout, "\n");
	}

	fprintf(stdout, "subsets in lexicographic order\n");
	int u;
	do u = rand()%8; while(u<4);
	bits(u); fprintf(stdout, ":\n");
	int s = 1;
	for(int i=0; i<8; i++) {
		fprintf(stdout, "%d (", s); bits(s);
		fprintf(stdout, ")\n");
		s = nextSubset(u, s);
	}

	fprintf(stdout, "bitwise negation\n");
	for(int i=0; i<10; i++) {
		int x = rand()%8;
		fprintf(stdout, "~%d (", x);
		bits(x);
		fprintf(stdout, ") = %d (", neg(x));
		bits(neg(x));
		fprintf(stdout, ")\n");
	}

	fprintf(stdout, "identify zero byte\n");
	for(int i=0; i<10; i++) {
		int x = rand()%8;
		fprintf(stdout, "%d is null: %d\n", x, isnull(x));
	}

	fprintf(stdout, "identify 0xa byte\n");
	for(int i=0; i<20; i++) {
		int x = rand()%16;
		fprintf(stdout, "%02d is linebreak: %d\n",
		                     x, isnull(x^NLBYTE));
	}
	return 0;
}

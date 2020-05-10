from sympy.polys.galoistools import (
    gf_from_int_poly, gf_to_int_poly,
    gf_lshift, gf_add_mul, gf_mul,
    gf_div, gf_rem,
    gf_gcdex,
    gf_sqf_p,
    gf_factor_sqf, gf_factor)

from sympy.polys.densebasic import (
    dup_LC, dmp_LC, dmp_ground_LC,
    dup_TC,
    dup_convert, dmp_convert,
    dup_degree, dmp_degree,
    dmp_degree_in, dmp_degree_list,
    dmp_from_dict,
    dmp_zero_p,
    dmp_one,
    dmp_nest, dmp_raise,
    dup_strip,
    dmp_ground,
    dup_inflate,
    dmp_exclude, dmp_include,
    dmp_inject, dmp_eject,
    dup_terms_gcd, dmp_terms_gcd)

from sympy.polys.densearith import (
    dup_neg, dmp_neg,
    dup_add, dmp_add,
    dup_sub, dmp_sub,
    dup_mul, dmp_mul,
    dup_sqr,
    dmp_pow,
    dup_div, dmp_div,
    dup_quo, dmp_quo,
    dmp_expand,
    dmp_add_mul,
    dup_sub_mul, dmp_sub_mul,
    dup_lshift,
    dup_max_norm, dmp_max_norm,
    dup_l1_norm,
    dup_mul_ground, dmp_mul_ground,
    dmp_quo_ground)

from sympy.polys.densetools import (
    dup_clear_denoms, dmp_clear_denoms,
    dup_trunc, dmp_ground_trunc,
    dup_content,
    dup_monic, dmp_ground_monic,
    dup_primitive, dmp_ground_primitive,
    dmp_eval_tail,
    dmp_eval_in, dmp_diff_eval_in,
    dmp_compose,
    dup_shift, dup_mirror)
from sympy.polys.euclidtools import (
    dmp_primitive,
    dup_inner_gcd, dmp_inner_gcd)

from sympy.polys.sqfreetools import (
    dup_sqf_p,
    dup_sqf_norm, dmp_sqf_norm,
    dup_sqf_part, dmp_sqf_part)

from sympy.polys.polyutils import _sort_factors
from sympy.polys.polyconfig import query

from sympy.polys.polyerrors import (
    ExtraneousFactors, DomainError, CoercionFailed, EvaluationFailed)

from sympy.ntheory import nextprime, isprime, factorint
from sympy.utilities import subsets # , cythonized

from math import ceil as _ceil, log as _log

def dup_zz_hensel_step(m, f, g, h, s, t, K):
    """
    One step in Hensel lifting in `Z[x]`.

    Given positive integer `m` and `Z[x]` polynomials `f`, `g`, `h`, `s`
    and `t` such that::

        f == g*h (mod m)
        s*g + t*h == 1 (mod m)

        lc(f) is not a zero divisor (mod m)
        lc(h) == 1

        deg(f) == deg(g) + deg(h)
        deg(s) < deg(h)
        deg(t) < deg(g)

    returns polynomials `G`, `H`, `S` and `T`, such that::

        f == G*H (mod m**2)
        S*G + T**H == 1 (mod m**2)

    References
    ==========

    1. [Gathen99]_

    """
    M = m**2

    e = dup_sub_mul(f, g, h, K)
    e = dup_trunc(e, M, K)

    q, r = dup_div(dup_mul(s, e, K), h, K)

    q = dup_trunc(q, M, K)
    r = dup_trunc(r, M, K)

    u = dup_add(dup_mul(t, e, K), dup_mul(q, g, K), K)
    G = dup_trunc(dup_add(g, u, K), M, K)
    H = dup_trunc(dup_add(h, r, K), M, K)

    u = dup_add(dup_mul(s, G, K), dup_mul(t, H, K), K)
    b = dup_trunc(dup_sub(u, [K.one], K), M, K)

    c, d = dup_div(dup_mul(s, b, K), H, K)

    c = dup_trunc(c, M, K)
    d = dup_trunc(d, M, K)

    u = dup_add(dup_mul(t, b, K), dup_mul(c, G, K), K)
    S = dup_trunc(dup_sub(s, d, K), M, K)
    T = dup_trunc(dup_sub(t, u, K), M, K)

    return G, H, S, T

def dup_zz_hensel_lift(p, f, f_list, l, K):
    """
    Multifactor Hensel lifting in `Z[x]`.

    Given a prime `p`, polynomial `f` over `Z[x]` such that `lc(f)`
    is a unit modulo `p`, monic pair-wise coprime polynomials `f_i`
    over `Z[x]` satisfying::

        f = lc(f) f_1 ... f_r (mod p)

    and a positive integer `l`, returns a list of monic polynomials
    `F_1`, `F_2`, ..., `F_r` satisfying::

       f = lc(f) F_1 ... F_r (mod p**l)

       F_i = f_i (mod p), i = 1..r

    References
    ==========

    1. [Gathen99]_

    """
    r = len(f_list)
    lc = dup_LC(f, K)

    if r == 1:
        F = dup_mul_ground(f, K.gcdex(lc, p**l)[0], K)
        return [ dup_trunc(F, p**l, K) ]

    m = p
    k = r // 2
    d = int(_ceil(_log(l, 2)))

    g = gf_from_int_poly([lc], p)


    for f_i in f_list[:k]:
        # print("g: %s * %s" % (g,f_i))
        g = gf_mul(g, gf_from_int_poly(f_i, p), p, K)

    # print("g: %s" % g)

    h = gf_from_int_poly(f_list[k], p)

    for f_i in f_list[k + 1:]:
        h = gf_mul(h, gf_from_int_poly(f_i, p), p, K)

    s, t, q = gf_gcdex(g, h, p, K)

    # print("gcdex %s %s %d = %s %s %s)" % (g,h,p,q,s,t))

    g = gf_to_int_poly(g, p)
    h = gf_to_int_poly(h, p)
    s = gf_to_int_poly(s, p)
    t = gf_to_int_poly(t, p)

    # print("h: %s" % f_list[k])

    for _ in range(1, d + 1):
        # print("go %d %s %s %s %s %s" % (m, f, g, h, s, t))
        (g, h, s, t), m = dup_zz_hensel_step(m, f, g, h, s, t, K), m**2
    # print("go %d %s %s %s %s %s" % (m, f, g, h, s, t))

    return dup_zz_hensel_lift(p, g, f_list[:k], l, K) \
        + dup_zz_hensel_lift(p, h, f_list[k:], l, K)

def _test_pl(fc, q, pl):
    if q > pl // 2:
        q = q - pl
    if not q:
        return True
    return fc % q == 0

def dup_zz_zassenhaus(f, K):
    """Factor primitive square-free polynomials in `Z[x]`. """
    n = dup_degree(f)

    if n == 1:
        return [f]

    fc = f[-1]
    A = dup_max_norm(f, K) # maximum(map abs (coeffs))
    b = dup_LC(f, K)
    B = int(abs(K.sqrt(K(n + 1))*2**n*A*b))
    C = int((n + 1)**(2*n)*A**(2*n - 1))
    gamma = int(_ceil(2*_log(C, 2)))
    bound = int(2*gamma*_log(gamma))
    a = []
    print("%d %d %d" % (n, A, b))
    print("%d %f %d" % (2**n*A*b, K.sqrt(5), K(n+1)))
    # choose a prime number `p` such that `f` be square free in Z_p
    # if there are many factors in Z_p, choose among a few different `p`
    # the one with fewer factors
    for px in range(3, bound + 1):
        if not isprime(px) or b % px == 0:
            continue

        px = K.convert(px)

        F = gf_from_int_poly(f, px)

        if not gf_sqf_p(F, px, K):
            continue
        fsqfx = gf_factor_sqf(F, px, K)[1]
        a.append((px, fsqfx))
        if len(fsqfx) < 15 or len(a) > 4: # what if len is 0?
            break
    p, fsqf = min(a, key=lambda x: len(x[1]))

    l = int(_ceil(_log(2*B + 1, p)))

    print("BOUNDs: %d %d %d %d %d %d %d" % (A, b, B, C, gamma, bound, l))

    modular = [gf_to_int_poly(ff, p) for ff in fsqf]

    # print("modular: %s" % modular)
    g = dup_zz_hensel_lift(p, f, modular, l, K)

    # print("HENSEL: %s" % g)

    sorted_T = range(len(g))
    T = set(sorted_T)
    factors, s = [], 1
    pl = p**l

    while 2*s <= len(T):
        for S in subsets(sorted_T, s):
            # lift the constant coefficient of the product `G` of the factors
            # in the subset `S`; if it is does not divide `fc`, `G` does
            # not divide the input polynomial

            # print("ffac: %s, %s" % (f, S))


            if b == 1:
                q = 1
                for i in S:
                    q = q*g[i][-1]
                q = q % pl
                if not _test_pl(fc, q, pl):
                    continue
            else:
                G = [b]
                for i in S:
                    G = dup_mul(G, g[i], K)
                G = dup_trunc(G, pl, K)
                G1 = dup_primitive(G, K)[1]
                q = G1[-1]
                if q and fc % q != 0:
                    continue

            H = [b]
            S = set(S)
            T_S = T - S

            # print("%s = %s - %s" % (T_S, T, S)) 

            if b == 1:
                G = [b]
                for i in S:
                    G = dup_mul(G, g[i], K)
                G = dup_trunc(G, pl, K)

            for i in T_S:
                H = dup_mul(H, g[i], K)

            # print("H untrunked: %s (%s)" % (H, pl))
            H = dup_trunc(H, pl, K)

            G_norm = dup_l1_norm(G, K)  # sum(map abs coeffs)
            H_norm = dup_l1_norm(H, K)

            if G_norm*H_norm <= B:
                T = T_S
                sorted_T = [i for i in sorted_T if i not in S]

                tmp = H
                G = dup_primitive(G, K)[1]
                f = dup_primitive(H, K)[1]
                # print("PRIMITIVE H: %s (%s)" % (H, tmp))

                factors.append(G)
                b = dup_LC(f, K)

                break
        else:
            s += 1

    return factors + [f]

if __name__ == "__main__":
  from sympy.polys import ZZ

  """
  rs = dup_zz_hensel_lift(7, [1,24,191,504], [[1,0], [1,1], [1,2]],2, ZZ)
  print("RESULT (hensel):")
  print(rs)

  fs = dup_zz_zassenhaus([1,24,191,504],ZZ)
  print("RESULT (zassenhaus):")
  print(fs)

  print(dup_zz_zassenhaus([1,9,3,15,0,3],ZZ))
  print(dup_zz_zassenhaus([1,0,9,3,10,7],ZZ))
  print(dup_zz_zassenhaus([1,16,16,6,0,8],ZZ))
  # print(dup_zz_zassenhaus([1,4,5,2],ZZ))

  rs = dup_zz_hensel_lift(7, [30,180,150,0], [[1,0], [1,1], [1,5]],2, ZZ)
  print("RESULT (hensel):")
  print(rs)
  """

  # fs = dup_zz_zassenhaus([30,180,150,0],ZZ)
  fs = dup_zz_zassenhaus([3,5,2,0],ZZ)
  print("RESULT (zassenhaus):")
  print(fs)

  fs = dup_zz_zassenhaus([1,3,2,0],ZZ)
  print("RESULT (zassenhaus):")
  print(fs)

  fs = dup_zz_zassenhaus([1,20,127,288,180],ZZ)
  print("RESULT (zassenhaus):")
  print(fs)


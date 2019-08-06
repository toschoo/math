#
# check
# https://docs.sympy.org/1.0/_modules/sympy/polys/euclidtools.html
#
def degree(a):
    return len(a) - 1

def lc(a):
    return a[-1]

def zeros(n):
    return [0 for _ in range(n)]

def unzero(n):
    k = [x for x in n]
    d = degree(k)
    while d > 0 and k[-1] == 0:
          k = k[:d]
          d = degree(k)
    return k

def add(a,b):
    da = degree(a)
    db = degree(b)
    if da > db:
       b += zeros (da-db)
    elif db > da:
       a += zeros (db-da)
    return [x+y for x,y in zip(a,b)]

def neg(a):
    return [-x for x in a]

def sub(a,b):
    return add(a,neg(b))

def scale(x,a):
    return [x*y for y in a]

def mul(a,b):
    u = zeros(degree(b)+1)
    i = 0
    for x in a:
        v = [x*y for y in b]
        z = zeros(i)
        i += 1
        u = add(u,z+v)
    return u

def gcd(a,b):
    if b == 0:
       return a
    return gcd(b,a%b)

def ratio(a,b):
    g = gcd(a,b)
    return (a/g, b/g)

def div(a,b):
    if b == [0]:
       return 1/0
    q = [0]
    r = unzero(a)
    db = degree(b)
    while degree(r) > 0 and degree(r) >= degree(b):
          t = lc(r) / lc(b)
          ts = zeros(degree(r)-db) + [t]
          q = add(q,ts)
          r = unzero(sub(r, mul(ts,b)))
    return (q,r)

def prem(a,b):
    d = degree(a) - degree(b)
    l = lc(b)**(d+1)
    c = [l*x for x in a]
    return div(c,b)[1]

def subres2(a,b):
    q = [x for x in a]
    r = [x for x in b]
    d1 = degree(q) - degree(r)
    g1 = lc(q)
    p1  = -1
    first = True
    res = []
    while degree(r) > 0:
          print("(%s,%s)" % (q,r))
          # res += [r]
          d0 = d1
          d1 = degree(q) - degree(r)
          g0 = g1
          g1 = lc(r) 
          if first:
             first = False
             beta = (-1)**(d0+1)
          else:
             p0 = p1
             p1 = ((-g1)**(d0)) / (p0**(d0-1))
             beta = (-g0)*((p1)**(d1))
             print("g0: %s" % g0)
             print("g1: %s" % g1)
             print("d0: %s" % d0)
             print("d1: %s" % d1)
             print("p1: %s" % p1)
             print("b : %s" % beta)
          t = [(g1**(d1+1))*x for x in q]
          q = [x for x in r] # r <- q
          print("q : %s" % q)
          (_,r) = div(t,q)   # r <- lc(r)^(d+1)*q / r
          print("q : %s" % q)
          r = [x/beta for x in r]
    return res

def subres(f,g):
    n = degree(f)
    m = degree(g)

    if n < m:
        f, g = g, f
        n, m = m, n

    if not f:
        return []

    if not g:
        return [f]

    R = [f, g]
    d = n - m

    b = (-1)**(d + 1)

    h = prem(f, g)
    h = scale(b,h)

    l = lc(g)
    c = l**d

    c = -c

    while degree(h) > 0:
        k = degree(h)
        R.append(h)

        f, g, m, d = g, h, k, m - k

        print("%s %s %s %s %s %s" % (c,l,f,g,m,d))

        b = -l * c**d

        h = prem(f, g)
        h = [x/b for x in h]

        l = lc(g)

        if d > 1:        # abnormal case
            q = c**(d - 1)
            c = (-l)**d / q
        else:
            c = -l

    R.append(h)
    return R


if __name__ == "__main__":
   """
   a = [4,3,2]
   b = [5,1,1]
   print("%s + %s = %s" % (a,b,add(a,b)))
   print("%s - %s = %s" % (a,b,sub(a,b)))
   print("%s * %s = %s" % (a,b,mul(a,b)))
   (q,r) = div(a,b)
   print("%s / %s = (%s,%s)" % (a,b,q,r))
   print("%s * %s + %s = %s" % (q,b,r,add(mul(q,b),r)))

   x = 12
   y = 15
   (n,d) = ratio(x,y)
   print("%s / %s = (%s,%s)" % (x,y,n,d))
   """
   a = [4,3,2]

   a = [-5,2,8,-3,-3,0,1,0,1]
   b = [21,-9,-4,0,5,0,3]
   #(q,r) = div(a,b)
   #print("%s / %s = (%s,%s)" % (a,b,q,r))
   print("subres(%s,%s):" % (a,b))
   print("%s" % subres(a,b))
   #print("%s" % subres(a,b))

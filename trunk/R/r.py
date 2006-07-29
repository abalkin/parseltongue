"""Python interface to R

Access to R is provided by the 'r' object:
>>> r
Use r(...) or r.<name>

>>> r('c(1, 2, 3)')
r('c(1, 2, 3)')

>>> r.exp(1)
r('2.71828182845905')

Named R objects can be retrieved and assigned is a variety of ways:

>>> r.some_name = 42
>>> r["some.name"]
r('42')

Deleting r.<name> removes <name> from R global environment
>>> del r.some_name
>>> r.ls()
r('character(0)')

Note that when using dot syntax only names that are legal python
identifiers are allowed and '_'s are replaced with a '.'.  Arbitrary
names can be accessed using the bracket notation:
>>> r["<<-"]
r('.Primitive("<<-")')

R output is redirected to sys.stdout
>>> a = r.array(range(6), dim=(2,3))
>>> Print(a)
     [,1] [,2] [,3]
[1,]    0    2    4
[2,]    1    3    5
"""

try:
    import numpy
except ImportError:
    pass
else:
    __doc__ += """

NumPy Support
=============

>>> from numpy import arange, asarray
>>> x = r.array(arange(6), dim=(3,2))
>>> asarray(x)
array([[0, 3],
       [1, 4],
       [2, 5]])

Character arrays are conversted to object arrays of python strings:

>>> x = r.array(['a', 'b', 'c', 'aa', 'bb', 'cc'], dim=(2,3))
>>> asarray(x)

"""
try:
    import Numeric
except ImportError:
    pass
else:
    __doc__ += """

Numeric Support
===============

>>> from Numeric import arange, asarray
>>> x = r.array(arange(6), dim=(3,2))
>>> asarray(x)
array([[0, 3],
       [1, 4],
       [2, 5]])

"""
# Make sure R_HOME is set before importing _r
import os
if os.getenv('R_HOME') is None:
    from subprocess import Popen, PIPE
    os.putenv('R_HOME', Popen(["R", "RHOME"], stdout=PIPE).communicate()[0].strip())
    del Popen, PIPE
del os
from _r import Sexp as _Sexp, PrintValue as Print, error
import _r

class _Door(object):
    """r - access to R"""
    def __new__(class_):
        try:
            return class_._singleton
        except AttributeError:
            x = class_._singleton = object.__new__(class_)
            return x
    
    def __getattr__(self, name):
        try:
            return R.findVar(name.replace('_', '.'))
        except error, e:
            raise AttributeError, e[0]

    def __setattr__(self, name, value):
        r.assign(name.replace('_', '.'), value)

    def __delattr__(self, name):
        r.rm(name.replace('_', '.'))

    def __setitem(self, name, value):
        r.assign(name.replace('_', '.'), value)

    def __getitem__(self, name):
        try:
            return R.findVar(name)
        except error, e:
            raise KeyError, e[0]
        
    def __delitem__(self, name):
        r.rm(name)

    def __call__(self, expr):
        return r.eval(r.parse(text=expr), R.GlobalEnv())

    def __repr__(self):
        return 'Use r(...) or r.<name>'
    
r = _Door()

_typemap = {
    bool:  _r.LGLSXP,
    int:   _r.INTSXP,
    float: _r.REALSXP,
    complex: _r.CPLXSXP,
    str: _r.STRSXP,
    }
_inverse_typemap = dict((v,k) for k,v in _typemap.iteritems())

def op(x):
    def method(*args):
        return r[x](*args)
    method.__doc__ = "Perform R operation '%s'" % x
    return method
def rop(x):
    def method(a, b):
        return r[x](b,a)
    method.__doc__ = "Perform R operation '%s'" % x
    return method


class R(_Sexp):
    """Python class representing R objects

    >>> x = R([1,2,3])
    >>> x
    r('c(1, 2, 3)')

    Indexing follows R rules (1-based):
    >>> x[2]
    r('2')

    R objects behave like python numerical objects, but the operations
    are performed by R
    >>> x + x
    r('c(2, 4, 6)')

    >>> R(1) / r.c(2,0,1)
    r('c(0.5, Inf, 1)')

    Python '.' acts like R '$':

    >>> l = r.list(a=1, b=2)
    >>> l.a
    r('1')

    Python callables can be passed to R functions that expect an R
    function argument:
    >>> r.lapply(l, lambda x: x+2).b
    r('4')
    
    """
    def __new__(class_, pyo):
        x = class_.fromArrayInterface(pyo)
        if x is not NotImplemented:
            return x
        try:
            length = len(pyo)
        except TypeError:
            rtype = _typemap[type(pyo)]
            length = 1
            pyo = [pyo]
        else:
            # strings are special
            if isinstance(pyo, str):
                rtype = _r.STRSXP
                length = 1
                pyo = [pyo]
            else:
                rtype = max(_typemap[type(e)] for e in pyo)
        return _Sexp.__new__(class_, rtype, length, pyo)

    def __len__(self):
        """returns the length of the R object

        >>> len(r.integer(10))
        10
        """
        return _r.elt(r.length(self), 0)
    
    def __call__(self, *args, **kwds):
        """forwards calls to R

        Arguments are converted to R objects
        >>> m = r.matrix(r.seq(10, 60, by=10), nrow = 2, ncol=3, byrow=True)
        >>> Print(r.apply(m, 1, lambda x: x/10))
             [,1] [,2]
        [1,]    1    4
        [2,]    2    5
        [3,]    3    6

        If an R function returns a Python objects wrapper, that object
        is returned:

        Compare:
        >>> r.foo = lambda: 42
        >>> r.foo()
        42

        to
        >>> r.bar = r("function() 42")
        >>> r.bar()
        r('42')
        
        """
        if _r.typeof(self) == _r.PROMSXP:
            self = self.fulfill()
        res = _Sexp.__call__(self, *map(asR, args),
                             **dict((k,asR(v)) for (k,v) in kwds.iteritems()))
        try:
            return _r.getpythonobject(res)
        except TypeError:
            return res

    def fulfill(self):
        return r['('](self)
    
    def __repr__(self):
        return 'r(%r)' % str(self)

    def __str__(self):
        return _Sexp.__str__(r.deparse(self))

    def __getitem__(self, i):
        try:
            return r["["](self, i)
        except error, e:
            raise IndexError, e[0]

    def __getattr__(self, a):
        try:
            return r["$"](self, a)
        except error, e:
            raise AttributeError, e[0]

    __pos__ = __add__ = __radd__ = op("+")
    __neg__ = __sub__ =  op("-"); __rsub__ = rop("-")
    __floordiv__ = op("%/%"); __rfloordiv__ = rop("%/%")
    __div__ = __truediv__ = op("/"); __rdiv__ = __rtruediv__ = rop("/")
    __mod__ = op("%%"); __rmod__ = rop("%%")
    __mul__ = op("*"); __rmul__ = rop("*")
    __pow__ = op("^"); __rpow__ = rop("^")
    __not__ = op("!")
    __or__ = op("|"); __ror__ = rop("|")
    __and__ = op("^"); __rand__ = rop("^")
    __xor__ = op("xor"); __rxor__ = rop("xor")
    __lt__ = op("<"); __rlt__ = rop("<")
    __gt__ = op(">"); __rgt__ = rop(">")
    __le__ = op("<="); __rle__ = rop("<=")
    __ge__ = op(">="); __rge__ = rop(">=")
    __eq__ = op("=="); __req__ = rop("==")
    __ne__ = op("!="); __rne__ = rop("!=")
    __abs__ = op("abs")
del op, rop

def asR(pyo):
    """convert to R if necessary"""
    # if already R - pass through
    if isinstance(pyo, R):
        return pyo
    # make callable objects callable in R
    if getattr(pyo, '__call__', None) is not None:
        f = r("function(type, x) function(...) .External('.Python', type, x, ...)")
        return f(R.PyObject(R), R.PyObject(pyo))
    return R(pyo)

def _test():
    import doctest
    doctest.testmod()
    
if __name__ == "__main__":
    _test()
                                

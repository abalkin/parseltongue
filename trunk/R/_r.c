/* Sexp objects */

#include "Python.h"
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rinterface.h>
#include <R_ext/Parse.h>
#include <R_ext/Rdynload.h>

#include <signal.h>

static PyObject *ErrorObject;
static SEXP GetErrMessage_SEXP;
static char*
geterrmessage(void)
{
	return CHARACTER_VALUE(eval(GetErrMessage_SEXP, R_GlobalEnv));
}

static PyObject*
exception_from_errmessage(void)
{
	PyErr_SetString(ErrorObject, geterrmessage());
	return NULL;
}

/* The Python original SIGINT handler */
PyOS_sighandler_t python_sigint;

/* Indicates whether the R interpreter was interrupted by a SIGINT */
int interrupted = 0;

/* Abort the current R computation due to a SIGINT */
static void
interrupt_R(int signum)
{
	interrupted = 1;
	error("Interrupted");
}


/* Evaluate a SEXP. It must be constructed by hand. It raises a Python
   exception if an error ocurred in the evaluation */
SEXP do_eval_expr(SEXP e) {
	SEXP res;
	int error = 0;
	PyOS_sighandler_t old_int;

	/* Enable our handler for SIGINT inside the R
	   interpreter. Otherwise, we cannot stop R calculations, since
	   SIGINT is only processed between Python bytecodes. Also, save the
	   Python SIGINT handler because it is necessary to temporally
	   restore it in user defined I/O Python functions. */
	/* stop_events(); */
  
#ifdef _WIN32
	old_int = PyOS_getsig(SIGBREAK);
#else
	old_int = PyOS_getsig(SIGINT);
#endif
	python_sigint = old_int;
	
	signal(SIGINT, interrupt_R);

	interrupted = 0;
	res = R_tryEval(e, R_GlobalEnv, &error);

#ifdef _WIN32
	PyOS_setsig(SIGBREAK, old_int);   
#else 
	PyOS_setsig(SIGINT, old_int);
#endif

	/* start_events(); */

	if (error) {
		if (interrupted)
			PyErr_SetNone(PyExc_KeyboardInterrupt);
		else
			exception_from_errmessage();
		return NULL;
	}
	
	return res;
}

typedef struct {
	PyObject_HEAD
	SEXP sexp;
} SexpObject;

/** Array interface **/

/* Array Interface flags */
#define FORTRAN       0x002
#define ALIGNED       0x100
#define NOTSWAPPED    0x200
#define WRITEABLE     0x400

typedef struct {
	int version;
	int nd;
	char typekind;
	int itemsize;
	int flags;
	Py_intptr_t *shape;
	Py_intptr_t *strides;
	void *data;
} PyArrayInterface;

static int
sexp_typekind(SEXP sexp)
{
	switch (TYPEOF(sexp)) {
	case REALSXP: return 'f';
	case INTSXP: return 'i';
	case STRSXP: return 'O';
	case CPLXSXP: return 'c';
	}
	return 0;
}

static int
sexp_itemsize(SEXP sexp)
{
	switch (TYPEOF(sexp)) {
	case REALSXP: return sizeof(*REAL(sexp));
	case INTSXP: return sizeof(*INTEGER(sexp));
	case STRSXP: return sizeof(PyObject*);
	case CPLXSXP: return sizeof(*COMPLEX(sexp));
	}
	return 0;
}

static int
sexp_rank(SEXP sexp)
{
	SEXP dim = getAttrib(sexp, R_DimSymbol);
	if (dim == R_NilValue)
		return 1;
	return LENGTH(dim);
}

static void
sexp_shape(SEXP sexp, Py_intptr_t* shape, int nd)
{
	int i;
	SEXP dim = getAttrib(sexp, R_DimSymbol);
	if (dim == R_NilValue)
		shape[0] = LENGTH(sexp);
	else for (i = 0; i < nd; ++i) {
		shape[i] = INTEGER(dim)[i];
	}
}

static void
array_struct_free(void *ptr, void *arr)
{
	PyArrayInterface *inter	= (PyArrayInterface *)ptr;
	free(inter->shape);
        free(inter);
        Py_DECREF((PyObject *)arr);
}

static void
array_struct_free_object(void *ptr, void *arr)
{
	PyArrayInterface *inter	= (PyArrayInterface *)ptr;
	Py_intptr_t length = 1, i;
	for (i = 0; i < inter->nd; ++i) 
		length *= inter->shape[i];
	/* let's leak for now
	for (i = 0; i < length; ++i)
		Py_DECREF(((PyObject **)inter->data)[i]);
	free(inter->data);
	*/
	free(inter->shape);
        free(inter);
}

static PyObject *
Sexp_array_struct_get(SexpObject *self)
{
	SEXP sexp = self->sexp;
	if (!sexp) {
		PyErr_SetString(PyExc_AttributeError, "Null sexp");
		return NULL;
	}
        PyArrayInterface *inter;
	int typekind =  sexp_typekind(sexp);
	if (!typekind) {
		PyErr_SetString(PyExc_AttributeError, "Unsupported SEXP type");
		return NULL;
	}
        if (!(inter = (PyArrayInterface *)malloc(sizeof(PyArrayInterface))))
		goto fail_inter;
	int nd = sexp_rank(sexp), i;
        inter->version = 2;
        inter->nd = nd;
        inter->typekind = typekind;
        inter->itemsize = sexp_itemsize(sexp);
        inter->flags = FORTRAN|ALIGNED|NOTSWAPPED|WRITEABLE;
        if (!(inter->shape = (Py_intptr_t*)malloc(sizeof(Py_intptr_t)*nd*2)))
		goto fail_shape;
	sexp_shape(sexp, inter->shape, nd);
        inter->strides = inter->shape + nd;
	Py_intptr_t stride = inter->itemsize;
	inter->strides[0] = stride;
	for (i = 1; i < nd; ++i) {
		stride *= inter->shape[i-1];
		inter->strides[i] = stride;
	}
	if (typekind == 'O') {
		R_len_t length = LENGTH(sexp);
		if (!(inter->data = malloc(sizeof(PyObject*)*length)))
			goto fail_data;
		for (i = 0; i < length; ++i) {
			PyObject* str = PyString_FromString(CHAR(STRING_ELT(sexp, i)));
			if (!str) {
				while (--i > 0)
					Py_DECREF(((PyObject **)inter->data)[i]);
				goto fail;
			}
			((PyObject **)inter->data)[i] = str;
		}
		return PyCObject_FromVoidPtrAndDesc(inter, self, array_struct_free_object);
	}
	else {
		inter->data = RAW(sexp);
		Py_INCREF(self);
		return PyCObject_FromVoidPtrAndDesc(inter, self, array_struct_free);
	}
 fail:
	free(inter->data);
 fail_data:
	free(inter->shape);
 fail_shape:
	free(inter);
 fail_inter:
	return PyErr_NoMemory();
}

/*********************/

static PyTypeObject Sexp_Type;

#define SexpObject_Check(v)	PyObject_IsInstance((PyObject*)v, (PyObject*)&Sexp_Type)

static int
Get_SEXP(PyObject* op, void* addr)
{
	if (SexpObject_Check(op)) {
		*(SEXP*)addr = ((SexpObject*)op)->sexp;
		return 1;
	}
	PyErr_SetString(PyExc_TypeError, "not an R object");
	return 0;
}

static SexpObject *
newSexpObject(PyTypeObject* type, SEXP arg)
{
	SexpObject *self;
	self = (SexpObject *)type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;
	self->sexp = arg;
	if (arg)
		R_PreserveObject(arg);
	return self;
}

/* Sexp methods */

static void
Sexp_dealloc(SexpObject *self)
{
	if (self->sexp)
		R_ReleaseObject(self->sexp);
	self->ob_type->tp_free((PyObject*)self);
}

/* NB: requires nd > 0 */
static Py_intptr_t
array_length(Py_intptr_t* dims, int nd)
{
	Py_intptr_t length = dims[0];
	int i;
	for (i = 1; i < nd; ++i) {
		length *= dims[i];
	}
	return length;
}

static int
array_ravel(char* dest, char* src, 
	    Py_intptr_t* strides, Py_intptr_t* dimensions, 
	    int nd, int itemsize, Py_intptr_t deststride)
{
	if (nd == 0) {
		memcpy(dest, src, itemsize);
		return 0;
	}
	int i;
	for (i = 0; i < *dimensions; ++i, src += *strides, dest += deststride) {
		if (-1 == array_ravel(dest, src, 
				      strides+1, dimensions+1,
				      nd-1, itemsize, deststride*(*dimensions)))
			return -1;
	}
	return 0;
}

static SexpObject *
Sexp_fromArrayInterface(PyTypeObject *type, PyObject *arg)
{
	SEXP sexp = 0;
	PyArrayInterface *inter;
	PyObject *attr;
    
	attr = PyObject_GetAttrString(arg, "__array_struct__");
	if (attr == NULL) {
		PyErr_Clear();
		Py_INCREF(Py_NotImplemented);
		return (SexpObject*)Py_NotImplemented;
	}
	if (!PyCObject_Check(attr) 
	    || ((inter=((PyArrayInterface *)PyCObject_AsVoidPtr(attr)))->version != 2)) 
		{
			PyErr_SetString(PyExc_ValueError, "invalid __array_struct__");
			return NULL;
		}
	int nd = inter->nd;
	if (nd < 1) {
		PyErr_Format(PyExc_ValueError, 
			     "length-%d dimension vector is invalid", nd);
		return NULL;
	}
	Py_intptr_t length = array_length(inter->shape, nd);
	switch (inter->typekind) {
	case 'f':
		if (inter->itemsize == sizeof(*REAL(sexp))) {
			sexp = allocVector(REALSXP, length);
		}
		break;
	case 'i':
		if (inter->itemsize == sizeof(*INTEGER(sexp))) {
			sexp = allocVector(INTSXP, length);
		}
		break;
	case 'c':
		if (inter->itemsize == sizeof(*COMPLEX(sexp))) {
			sexp = allocVector(CPLXSXP, length);
		}
		break;
	}
	if (!sexp) {
		PyErr_Format(PyExc_ValueError, 
			     "cannot handle type '%c' and item size %d",
			     inter->typekind, inter->itemsize);
	}
	array_ravel(RAW(sexp), inter->data, inter->strides, inter->shape,
		    nd, inter->itemsize, inter->itemsize);
	if (nd > 1) {
		/* set dim attribute */
		SEXP dim = allocVector(INTSXP, nd);
		PROTECT(dim);
		int i;
		for (i = 0; i < nd; ++i) {
			INTEGER(dim)[i] = inter->shape[i];
		}
		setAttrib(sexp, R_DimSymbol, dim);
		UNPROTECT(1);
	}
	return newSexpObject(type, sexp);
}


static SexpObject *
Sexp_findVar(PyTypeObject *type, PyObject *args)
{
	char *name;
	SEXP rho = R_GlobalEnv, res;
	if (!PyArg_ParseTuple(args, "s|O&", &name, Get_SEXP, &rho)) {
		return NULL;
	}
	res = findVar(install(name), rho);
	if (res != R_UnboundValue)
		return newSexpObject(type, res);
	PyErr_Format(ErrorObject, "'%s' not found", name);
	return NULL;
}

static PyObject*
Sexp_nil(PyTypeObject* type)
{
	SexpObject* res = (SexpObject*)type->tp_alloc(type, 0);
	res->sexp = R_NilValue;
	return (PyObject*)res;
}

static PyObject*
Sexp_GlobalEnv(PyTypeObject* type)
{
	SexpObject* res = (SexpObject*)type->tp_alloc(type, 0);
	res->sexp = R_GlobalEnv;
	return (PyObject*)res;
}


static SEXP mkPyObject(PyObject*);
static PyObject*
Sexp_PyObject(PyTypeObject* type, PyObject* pyo)
{
	return (PyObject*)newSexpObject(type, mkPyObject(pyo));
}

static PyMethodDef Sexp_methods[] = {
	{"fromArrayInterface",	(PyCFunction)Sexp_fromArrayInterface,	METH_O|METH_CLASS,
		PyDoc_STR("fromArrayInterface(array) -> Sexp")},
	{"findVar",	(PyCFunction)Sexp_findVar,	METH_VARARGS|METH_CLASS,
		PyDoc_STR("findVar(name, env) -> Sexp")},
	{"nil",	(PyCFunction)Sexp_nil,	METH_NOARGS|METH_CLASS,
		PyDoc_STR("nil() -> NULL")},
	{"GlobalEnv",	(PyCFunction)Sexp_GlobalEnv,	METH_NOARGS|METH_CLASS,
		PyDoc_STR("GlobalEnv() -> GlobalEnv")},
	{"PyObject",   (PyCFunction)Sexp_PyObject,   METH_O|METH_CLASS,
	        PyDoc_STR("PyObject(pyo) -> R")},
	{NULL,		NULL}		/* sentinel */
};

static PyObject*
Sexp_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
	static char* kwnames[] = {"type", "length", "data"};
	SexpObject* res = (SexpObject*)type->tp_alloc(type, 0);
	int rtype = -1, length = -1, datalength, i;
	PyObject* seq = 0, *fast_seq, *item;
	SEXP sexp;
	if (!res)
		return PyErr_NoMemory();
	if (!PyArg_ParseTupleAndKeywords(args, kwds, "iiO:new", kwnames,
					 &rtype, &length, &seq))
		return NULL;
	fast_seq = PySequence_Fast(seq, "Sexp.__new__: data must be a sequence");
	if (!fast_seq)
		return NULL;
	sexp = res->sexp = allocVector(rtype, length);
	if (!sexp) {
		Py_DECREF(res);
		return PyErr_NoMemory();
	}
	R_PreserveObject(sexp);
	datalength = PySequence_Fast_GET_SIZE(fast_seq);
	if (length == -1)
		length = datalength;
	if (datalength == 0 && length != 0) {
		PyErr_Format(PyExc_ValueError, "Sexp.__new__: cannot create"
			     " object of length %d from data of length %d",
			     length, datalength);
		return NULL;
	}
	switch(rtype) {
	case REALSXP:
		for (i = 0; i < length; ++i) {
			if((item = PyNumber_Float(PySequence_Fast_GET_ITEM(fast_seq, i%datalength)))) {
				REAL(sexp)[i] = PyFloat_AS_DOUBLE(item);
				Py_DECREF(item);
			}
			else {
				PyErr_Clear();
				REAL(sexp)[i] = NA_REAL;
			}
		}
		break;
	case INTSXP:
		for (i = 0; i < length; ++i) {
			if((item = PyNumber_Int(PySequence_Fast_GET_ITEM(fast_seq, i%datalength)))) {
				long l = PyInt_AS_LONG(item);
				INTEGER(sexp)[i] = (l<=INT_MAX && l>=INT_MIN)?l:NA_INTEGER;
				Py_DECREF(item);
			}
			else {
				PyErr_Clear();
				INTEGER(sexp)[i] = NA_INTEGER;
			}
		}
		break;
	case LGLSXP:
		for (i = 0; i < length; ++i) {
			int q = PyObject_IsTrue(PySequence_Fast_GET_ITEM(fast_seq, i%datalength));
			if (q != -1)
				LOGICAL(sexp)[i] = q;
			else {
				PyErr_Clear();
				LOGICAL(sexp)[i] = NA_LOGICAL;
			}
		}
		break;
	case STRSXP:
		for (i = 0; i < length; ++i) {
			if((item = PyObject_Str(PySequence_Fast_GET_ITEM(fast_seq, i%datalength)))) {
				SEXP rstr = mkChar(PyString_AS_STRING(PySequence_ITEM(seq, i%datalength)));
				if (!rstr) {
					PyErr_NoMemory();
					Py_DECREF(res); 
					res = NULL;
					break;
				}
				SET_STRING_ELT(sexp, i, rstr);
			}
			else {
				PyErr_Clear();
				SET_STRING_ELT(sexp, i, NA_STRING);
			}
		}
		break;
	default:
		PyErr_Format(PyExc_ValueError, "cannot handle type %d", rtype);
		Py_DECREF(res);
		res = NULL;
	}
	
	if (fast_seq != seq) {
		Py_DECREF(fast_seq);
	}
	return (PyObject*)res;
}

/* Convert a tuple to arguments for a R function */
static int
make_args(int largs, PyObject *args, SEXP *e)
{
	SEXP r;
	int i;

	for (i=0; i<largs; i++) {
		if (!Get_SEXP(PyTuple_GetItem(args, i), &r))
			return 0;
		SETCAR(*e, r);
		*e = CDR(*e);
	}
	return 1;
}

/* Implements the conversion rules for names. See the 'USING' file. We
   don't care about '<-' because it doesn't appear in keywords. */
static char *
dotter(char *s)
{
	char *r, *res;
	int l;

	if (!s)
		return NULL;                /* assume prev PyString_AsString has failed */
	l = strlen(s);
	r = (char *)PyMem_Malloc(l+1);
	if (!r) {
		PyErr_NoMemory();
		return NULL;
	}
	res = strcpy(r, s); 

	if ((l > 1) && (res[l-1] == '_') && (res[l-2] != '_'))
		res[l-1]=0;

	while ((r=strchr(r, '_')))
		*r = '.';
	
	return res;
}


/* Convert a dict to keywords arguments for a R function */
int
make_kwds(int lkwds, PyObject *kwds, SEXP *e)
{
	SEXP r;
	char *s;
	int i, q;
	PyObject *citems=NULL, *it;
	PyObject *kwname;

	if (kwds) {
		citems = PyMapping_Items(kwds);
	}

	for (i=0; i<lkwds; i++) {
		it = PySequence_GetItem(citems, i);
		if (!it)
			goto fail;
		q = Get_SEXP(PyTuple_GetItem(it, 1), &r);
		Py_DECREF(it);
		if (!q)
			goto fail;

		SETCAR(*e, r);
		kwname = PyTuple_GetItem(it, 0);
		if (!kwname)
			goto fail;
		if (!PyString_Check(kwname)) {
			PyErr_SetString(PyExc_TypeError, "keywords must be strings");
			goto fail;
		}
		s = dotter(PyString_AsString(kwname));
		if (!s)
			goto fail;

		SET_TAG(*e, install(s));
		PyMem_Free(s);
		*e = CDR(*e);
	}
	Py_XDECREF(citems);
	return 1;

 fail:
	Py_XDECREF(citems);
	return 0;
}

/* This is the method to call when invoking an 'Sexp' */
static PyObject *
Sexp_call(PyObject *self, PyObject *args, PyObject *kwds)
{
	SEXP exp, e, res;
	int largs, lkwds;

	largs = lkwds = 0;
	if (args)
		largs = PyObject_Length(args);
	if (kwds)
		lkwds = PyObject_Length(kwds);
	if ((largs<0) || (lkwds<0))
		return NULL;

	/* A SEXP with the function to call and the arguments and keywords. */
	PROTECT(exp = allocVector(LANGSXP, largs+lkwds+1));
	e = exp;
	SETCAR(e, ((SexpObject *)self)->sexp);
	e = CDR(e);

	if (!make_args(largs, args, &e)) {
		UNPROTECT(1);
		return NULL;
	}
	if (!make_kwds(lkwds, kwds, &e)) {
		UNPROTECT(1);
		return NULL;
	}

	PROTECT(res = do_eval_expr(exp));
	if (!res) {
		UNPROTECT(2);
		return NULL;
	}
	UNPROTECT(2);
	extern void Rf_PrintWarnings(void);
	Rf_PrintWarnings(); /* show any warning messages */
  
	return (PyObject*)newSexpObject(self->ob_type, res);
}


static PyObject*
Sexp_str(PyObject* self)
{
	return PyString_FromString(CHARACTER_VALUE(((SexpObject*)self)->sexp));
}

static PyGetSetDef Sexp_getset[] = {
	{"__array_struct__", (getter)Sexp_array_struct_get, NULL,
         "Array protocol: struct"},
	{NULL, NULL, NULL, NULL},  /* Sentinel */
};	

static PyTypeObject Sexp_Type = {
	/* The ob_type field must be initialized in the module init function
	 * to be portable to Windows without using C++. */
	PyObject_HEAD_INIT(NULL)
	0,			/*ob_size*/
	"_r.Sexp",		/*tp_name*/
	sizeof(SexpObject),	/*tp_basicsize*/
	0,			/*tp_itemsize*/
	/* methods */
	(destructor)Sexp_dealloc, /*tp_dealloc*/
	0,			/*tp_print*/
	0,                      /*tp_getattr*/
	0,                      /*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
        Sexp_call,              /*tp_call*/
        Sexp_str,               /*tp_str*/
        0,                      /*tp_getattro*/
        0,                      /*tp_setattro*/
        0,                      /*tp_as_buffer*/
        Py_TPFLAGS_DEFAULT|Py_TPFLAGS_BASETYPE,     /*tp_flags*/
        0,                      /*tp_doc*/
        0,                      /*tp_traverse*/
        0,                      /*tp_clear*/
        0,                      /*tp_richcompare*/
        0,                      /*tp_weaklistoffset*/
        0,                      /*tp_iter*/
        0,                      /*tp_iternext*/
        Sexp_methods,           /*tp_methods*/
        0,                      /*tp_members*/
        Sexp_getset,            /*tp_getset*/
        0,                      /*tp_base*/
        0,                      /*tp_dict*/
        0,                      /*tp_descr_get*/
        0,                      /*tp_descr_set*/
        0,                      /*tp_dictoffset*/
        0,                      /*tp_init*/
        0,                      /*tp_alloc*/
        Sexp_new,                      /*tp_new*/
        0,                      /*tp_free*/
        0,                      /*tp_is_gc*/
};
/* --------------------------------------------------------------------- */
PyDoc_STRVAR(rpyobject_PrintValue_doc,
"PrintValue(sexp)\n\
\n\
Print R object using normal R printing mechanism");

static PyObject *
rpyobject_PrintValue(PyObject *self,  PyObject *arg)
{
	if (!SexpObject_Check(arg)) {
		PyErr_SetString(PyExc_TypeError, "not an sexp");
		return NULL;
	}
	PrintValue(((SexpObject*)arg)->sexp);
	Py_RETURN_NONE;
}

PyDoc_STRVAR(rpyobject_elt_doc,
"elt(sexp, i)\n\
\n\
Returns i-th element as a native Python object");

static PyObject *
rpyobject_elt(PyObject *self,  PyObject *args)
{
	PyObject *res;
	SEXP sexp;
	long i;
	if (!PyArg_ParseTuple(args, "O&l", Get_SEXP, &sexp, &i)) {
		return NULL;
	}
	if (i < 0 || i >= LENGTH(sexp)) {
		PyErr_SetString(PyExc_IndexError, "index out of range");
		return NULL;
	}
	switch (TYPEOF(sexp)) {
	case REALSXP:
		res = PyFloat_FromDouble(REAL(sexp)[i]);
		break;
	case INTSXP:
		res = PyInt_FromLong(INTEGER(sexp)[i]);
		break;
	case LGLSXP:
		res = PyBool_FromLong(LOGICAL(sexp)[i]);
		break;
	case STRSXP:
		res = PyString_FromString(CHAR(STRING_ELT(sexp, i)));
		break;
	default:
		PyErr_Format(PyExc_TypeError, "cannot handle R type %d", TYPEOF(sexp));
		res = NULL;
	}
	return res;
}

PyDoc_STRVAR(rpyobject_geterrmessage_doc,
"geterrmessage()\n\
\n\
Last error message");

static PyObject *
rpyobject_geterrmessage(PyObject *self)
{
	return PyString_FromString(geterrmessage());
}

PyDoc_STRVAR(rpyobject_getpythonobject_doc,
"getpythonobject(SEXP) -> python object\n\
\n\
Extract python object from SEXP");

static PyObject * rpyobject_getpythonobject(PyObject *, PyObject *);

PyDoc_STRVAR(rpyobject_typeof_doc,
"typeof(SEXP) -> int\n\
\n\
Returns internal SEXP type");

static PyObject*
rpyobject_typeof(PyObject *self, PyObject* arg)
{
	return PyInt_FromLong(TYPEOF(((SexpObject*)arg)->sexp));
}


/* List of functions defined in the module */

static PyMethodDef rpyobject_methods[] = {
	{"PrintValue",		rpyobject_PrintValue,		METH_O,
	 	rpyobject_PrintValue_doc},
	{"elt",		        rpyobject_elt,	                METH_VARARGS,
	 	rpyobject_elt_doc},
	{"geterrmessage",	(PyCFunction)rpyobject_geterrmessage,	METH_NOARGS,
	 	rpyobject_geterrmessage_doc},
	{"getpythonobject",	(PyCFunction)rpyobject_getpythonobject,	METH_O,
	 	rpyobject_getpythonobject_doc},
	{"typeof",              (PyCFunction)rpyobject_typeof,  METH_O,
	        rpyobject_typeof_doc},
	{NULL,		NULL}		/* sentinel */
};

/* Calling Python from R */
/* ===================== */

/* R representation of a PyObject */
static SEXP R_PyObject_type_tag;

static SEXP
R_PyObject_decref(SEXP s)
{
	PyObject* pyo = (PyObject*)R_ExternalPtrAddr(s);
	if (pyo) {
		Py_DECREF(pyo);
		R_ClearExternalPtr(s);
	}
	return R_NilValue;
}

static SEXP
mkPyObject(PyObject* pyo)
{
	SEXP res;
	Py_INCREF(pyo);
	res = R_MakeExternalPtr(pyo, R_PyObject_type_tag, R_NilValue);
	R_RegisterCFinalizer(res, (R_CFinalizer_t)R_PyObject_decref);
	return res;
}

#define R_PyObject_TYPE_CHECK(s)			\
	(TYPEOF(s) == EXTPTRSXP &&  R_ExternalPtrTag(s) == R_PyObject_type_tag)
static PyObject *
rpyobject_getpythonobject(PyObject *self, PyObject* arg)
{
	SEXP s;
	PyObject* res;
	if (!SexpObject_Check(arg) 
	    || !R_PyObject_TYPE_CHECK(s = ((SexpObject*)arg)->sexp)) {
		PyErr_SetString(PyExc_TypeError, "Not a python object");
		return NULL;
	}
	res = (PyObject*)R_ExternalPtrAddr(s);
	Py_INCREF(res);
	return res;
}



static SEXP
do_Python(SEXP args)
{
	SEXP s = CADR(args), res;
	if (!R_PyObject_TYPE_CHECK(s)) {
		error(".Python: invalid python type");
		return R_NilValue;
	}
	PyTypeObject* type = R_ExternalPtrAddr(s);
	args = CDDR(args);
	s = CAR(args);
	if (!R_PyObject_TYPE_CHECK(s)) {
		error(".Python: invalid function");
		return R_NilValue;
	}
	PyObject *pyf = R_ExternalPtrAddr(s);

	/* create argument list */
	PyObject *pyargs = PyList_New(0), *pyres;
	for (args = CDR(args); args != R_NilValue; args = CDR(args)) {
		s = CAR(args);
		if (R_PyObject_TYPE_CHECK(s)) {
			PyList_Append(pyargs, (PyObject *)R_ExternalPtrAddr(s));
		}
		else {
			PyList_Append(pyargs, (PyObject *)newSexpObject(type, s));
		}
	}
	PyObject *pyargstup = PyList_AsTuple(pyargs);
	/* XXX named arguments are not supported yet */
	pyres = PyObject_Call(pyf, pyargstup, NULL);
	if (!pyres) {
		PyObject *exctype;
		PyObject *excvalue; 
		PyObject *exctraceback;
		PyObject *excstr;
		PyErr_Fetch(&exctype, &excvalue, &exctraceback);
		excstr = PyObject_Str(excvalue);
		if (excstr) {
			error(PyString_AS_STRING(excstr));
			Py_DECREF(excstr);
		} 
		else {
			error("Python error");
		}
		PyErr_Clear();
	}
	Py_DECREF(pyargs);
	Py_DECREF(pyargstup);
	if (SexpObject_Check(pyres)){
		res = ((SexpObject*)pyres)->sexp;
	}
	else {
		res = mkPyObject(pyres);
	}
	Py_DECREF(pyres);
	       
	return res;
}

static void
rpyobject_WriteConsole(char *buf, int len)
{
  PyOS_sighandler_t old_int;

  /* It is necessary to restore the Python handler when using a Python
     function for I/O. */
  old_int = PyOS_getsig(SIGINT);
  PyOS_setsig(SIGINT, python_sigint);
  PySys_WriteStdout(buf);
  signal(SIGINT, old_int);
}


static R_ExternalMethodDef externalMethods[] = { 
	{".Python", (DL_FUNC)&do_Python, -1},
	{NULL, NULL, 0} 
};

PyDoc_STRVAR(module_doc,
"SEXP wrapper.");

/* Initialization function for the module (*must* be called initrpyobject) */
#define ADD_INT_CONSTANT(module, name) PyModule_AddIntConstant(module, #name, name)
PyMODINIT_FUNC
init_r(void)
{
	extern int Rf_initEmbeddedR(int argc, char **argv);
	PyObject *m;
	char *defaultargv[] = {"rpy", "-q", "--vanilla"};

	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability to Windows 
	 * without requiring C++. */
	if (PyType_Ready(&Sexp_Type) < 0)
		return;

	/* Create the module and add the functions */
	m = Py_InitModule3("_r", rpyobject_methods, module_doc);
	if (m == NULL)
		return;
	PyModule_AddObject(m, "Sexp", (PyObject *)&Sexp_Type);

	/* Add some symbolic constants to the module */
	ADD_INT_CONSTANT(m, NILSXP);
	ADD_INT_CONSTANT(m, SYMSXP);
	ADD_INT_CONSTANT(m, LISTSXP);
	ADD_INT_CONSTANT(m, CLOSXP);
	ADD_INT_CONSTANT(m, ENVSXP);
	ADD_INT_CONSTANT(m, PROMSXP);
	ADD_INT_CONSTANT(m, LANGSXP);
	ADD_INT_CONSTANT(m, SPECIALSXP);
	ADD_INT_CONSTANT(m, BUILTINSXP);
	ADD_INT_CONSTANT(m, CHARSXP);
	ADD_INT_CONSTANT(m, STRSXP);
	ADD_INT_CONSTANT(m, LGLSXP);
	ADD_INT_CONSTANT(m, INTSXP);
	ADD_INT_CONSTANT(m, REALSXP);
	ADD_INT_CONSTANT(m, CPLXSXP);
	ADD_INT_CONSTANT(m, DOTSXP);
	ADD_INT_CONSTANT(m, ANYSXP);
	ADD_INT_CONSTANT(m, VECSXP);
	ADD_INT_CONSTANT(m, VECSXP);
	ADD_INT_CONSTANT(m, EXPRSXP);
	ADD_INT_CONSTANT(m, BCODESXP);
	ADD_INT_CONSTANT(m, EXTPTRSXP);
	ADD_INT_CONSTANT(m, RAWSXP);

	if (ErrorObject == NULL) {
		ErrorObject = PyErr_NewException("rpyobject.error", NULL, NULL);
		if (ErrorObject == NULL)
			return;
	}
	Py_INCREF(ErrorObject);
	PyModule_AddObject(m, "error", ErrorObject);
	Rf_initEmbeddedR( sizeof(defaultargv) / sizeof(defaultargv[0]),
			  defaultargv);
	R_PreserveObject(GetErrMessage_SEXP = allocVector(LANGSXP, 1));
	SETCAR(GetErrMessage_SEXP, findVar(install("geterrmessage"), R_GlobalEnv));
	/*
	R_registerRoutines(dll, cMethods, callMethods,
			   fortranMethods, externalMethods);
	*/
	extern DllInfo* getBaseDllInfo(void);
	R_registerRoutines(getBaseDllInfo(), NULL, NULL, NULL, externalMethods);
	R_PyObject_type_tag = install("PyObject_TYPE_TAG");

	/* Redirect R console output */
	R_Outputfile = NULL;
	extern void (*ptr_R_WriteConsole)(char *, int);
	ptr_R_WriteConsole = rpyobject_WriteConsole;
}

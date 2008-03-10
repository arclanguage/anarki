(require (lib "foreign.ss"))
(unsafe!)

(xdef 'ffi-lib ffi-lib)
(xdef 'get-ffi-obj get-ffi-obj)

(xdef 'cbyte _byte)
(xdef 'cshort _short)
(xdef 'cushort _ushort)
(xdef 'cint _int)
(xdef 'cuint _uint)
(xdef 'clong _long)
(xdef 'culong _ulong)

(xdef 'cfloat _float)
(xdef 'cdouble _double)

(xdef 'cbytes _bytes)
(xdef 'cvec _cvector)
(xdef 'cstring _string)

(xdef 'cvoid _void)
(xdef 'cfn _cprocedure)
(xdef 'csizeof ctype-sizeof)

(xdef 'cptr _pointer)
(xdef 'cpref ptr-ref)
(xdef 'cpset ptr-set!)
(xdef 'cmalloc malloc)
(xdef 'cfree free)
(xdef 'cfinalize register-finalizer)
(xdef 'gc collect-garbage)


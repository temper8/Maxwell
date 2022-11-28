import numpy
import fib1

print(fib1.fib.__doc__)
print(fib1.init_vi.__doc__)
print(fib1.init_fmaxw_classic.__doc__)
print(fib1.fmaxw_classic.__doc__)

#a = numpy.zeros(1002, 'd')
a = fib1.init_vi(12.3)
print(a)

f = fib1.init_fmaxw_classic(12.3, 0)
print(len(f))
print(f)

call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" 
::python -m numpy.f2py fib1.f90 -m fib1    -h fib1.pyf > log.txt
::python -m numpy.f2py -c fib1.f90 -m fib1  > log.txt
:: C:\Program Files (x86)\Intel\oneAPI\compiler\latest\windows\redist\intel64_win\compiler

python -m numpy.f2py -c --fcompiler=intelvem --compiler=msvc  fib1.pyf fib1.f90  > log.txt

echo compile fib1.f90 

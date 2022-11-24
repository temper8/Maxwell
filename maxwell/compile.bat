
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" 
python -m numpy.f2py -c fib1.f90 -m fib1  

echo compile fib1.f90 
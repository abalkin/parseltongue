
import os, os.path, sys, shutil
from distutils.core import setup, Extension
from subprocess import Popen, PIPE
RHOME = os.getenv("R_HOME")
if RHOME is None:
    RHOME = Popen(["R", "RHOME"], stdout=PIPE).communicate()[0].strip()

r_libs = [os.path.join(RHOME, 'lib')]
rpyobject = Extension(
        "_r",
        ["_r.c"],
        include_dirs=[ os.path.join(RHOME, 'include'),],
        libraries=['R'],
        library_dirs=r_libs,
        runtime_library_dirs=r_libs,
        extra_compile_args = ["-O0", "-g"],
        )
setup(name="rpyobject",
      version="0.0",
      description="Python interface to the R language",
      url="http://rpy.sourceforge.net",
      license="GPL",
      ext_modules=[rpyobject],
      py_modules=['r']
      )

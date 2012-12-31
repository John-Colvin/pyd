# DSR:2005.10.27.23.51

# XXX:
# These two will have to wait until DMD can create shared libraries on Linux,
# because DSR doesn't have (the non-free version of) MSVC 2003, which is
# necessary to create a debug build or a UCS4 build of Python 2.4 on Windows:
# - Handle distutils debug builds responsibly (make sure both -debug and -g are
#   passed through to DMD, even if optimizations are requested).  Also make
#   sure that extensions built with this module work under debug builds of
#   Python.
# - Try out a UCS4 build of Python to make sure that works.

import os, os.path, sys

from distutils import ccompiler as cc
from distutils.ccompiler import gen_lib_options
from distutils.errors import (
    DistutilsExecError, DistutilsFileError, DistutilsPlatformError,
    CompileError, LibError, LinkError, UnknownFileError
)

_isPlatCygwin = sys.platform.lower() == 'cygwin'

def winpath(path_, winonly):
    if _isPlatCygwin and winonly:
        from subprocess import Popen, PIPE
        stdout,_ = Popen(['cygpath', '-w', path_], stdout=PIPE).communicate()
        return stdout.strip()
    else:
        return path_
def cygpath(path_, winonly):
    if _isPlatCygwin and winonly:
        from subprocess import Popen, PIPE
        stdout,_ = Popen(['cygpath', path_], stdout=PIPE).communicate()
        return stdout.strip()
    else:
        return path_
_isPlatWin = sys.platform.lower().startswith('win') or _isPlatCygwin

_infraDir = os.path.join(os.path.dirname(__file__), 'infrastructure')

from celerid.pyd_support import make_pydmain, make_pyddef

_pydFiles = [
    'class_wrap.d',
    'ctor_wrap.d',
    'def.d',
    'embedded.d',
    'exception.d',
    'extra.d',
    'func_wrap.d',
    'make_object.d',
    'make_wrapper.d',
    'op_wrap.d',
    'pyd.d',
    'pydobject.d',
    'struct_wrap.d',
]

_metaFiles = [
    'Demangle.d',
    'Nameof.d',
]

_utilFiles = [
    'conv.d',
    'typelist.d',
    'multi_index.d',
    'replace.d',
]

_deimosFiles = [
    'abstract_.d',
    'ast.d',
    'boolobject.d',
    'bufferobject.d',
    'bytearrayobject.d',
    'bytesobject.d',
    'cellobject.d',
    'ceval.d',
    'classobject.d',
    'cobject.d',
    'codecs.d',
    'code.d',
    'compile.d',
    'complexobject.d',
    'cStringIO.d',
    'datetime.d',
    'descrobject.d',
    'dictobject.d',
    'enumobject.d',
    'errcode.d',
    'eval.d',
    'fileobject.d',
    'floatobject.d',
    'frameobject.d',
    'funcobject.d',
    'genobject.d',
    'grammar.d',
    'import_.d',
    'intobject.d',
    'intrcheck.d',
    'iterobject.d',
    'listobject.d',
    'longintrepr.d',
    'longobject.d',
    'marshal.d',
    'memoryobject.d',
    'methodobject.d',
    'modsupport.d',
    'moduleobject.d',
    'node.d',
    'object.d',
    'objimpl.d',
    'parsetok.d',
    'pgenheaders.d',
    'pyarena.d',
    'pyatomic.d',
    'pycapsule.d',
    'pydebug.d',
    'pyerrors.d',
    'pymem.d',
    'pyport.d',
    'pystate.d',
    'pystrcmp.d',
    'pystrtod.d',
    'Python.d',
    'pythonrun.d',
    'pythread.d',
    'rangeobject.d',
    'setobject.d',
    'sliceobject.d',
    'stringobject.d',
    'structmember.d',
    'structseq.d',
    'symtable.d',
    'sysmodule.d',
    'timefuncs.d',
    'traceback.d',
    'tupleobject.d',
    'unicodeobject.d',
    'weakrefobject.d',
]

_pyVerXDotY = '.'.join(str(v) for v in sys.version_info[:2]) # e.g., '2.4'
_pyVerXY = _pyVerXDotY.replace('.', '') # e.g., '24'

def spawn0(self, cmdElements):
    import platform
    if platform.python_version() < "2.6":
        eval("""
        try:
            self.spawn(cmdElements)
        except DistutilsExecError, msg:
            raise CompileError(msg)
        """)
    else:
        try:
            self.spawn(cmdElements)
        except DistutilsExecError as msg:
            raise CompileError(msg)

class DCompiler(cc.CCompiler):

    src_extensions = ['.d']
    obj_extension = (_isPlatWin and '.obj') or '.o'
    static_lib_extension = (_isPlatWin and '.lib') or '.a'
    shared_lib_extension = (_isPlatWin and '.pyd') or '.so'
    static_lib_format = (_isPlatWin and '%s%s') or 'lib%s%s'
    shared_lib_format = '%s%s'
    exe_extension = (_isPlatWin and '.exe') or ''

    def __init__(self, *args, **kwargs):
        cc.CCompiler.__init__(self, *args, **kwargs)
        self.winonly = False
        self.proj_name = None
        self.build_exe = False
        self.with_pyd = True
        self.with_main = True
        self.build_deimos = False
        self.optimize = False
        self.versionFlagsFromExt = []
        self.debugFlagsFromExt = []
        # Get DMD/GDC specific info
        self._initialize()
        # _binpath
        try:
            dBin = os.environ[self._env_var]
            if not os.path.isfile(dBin):
                self.warn("Environment variable %s provided, but file '%s' does not exist." % (self._env_var, dBin))
                raise KeyError
        except KeyError:
            if _isPlatWin:
                # The environment variable wasn't supplied, so search the PATH.
                # Windows requires the full path for reasons that escape me at
                # the moment.
                dBin = _findInPath(self.executables['compiler'][0] + self.exe_extension)
                if dBin is None:
                    raise DistutilsFileError('You must either set the %s'
                        ' environment variable to the full path of the %s'
                        ' executable, or place the executable on the PATH.' %
                        (self._env_var, self.executables['compiler'][0])
                    )
            else:
                # Just run it via the PATH directly in Linux
                dBin = self.executables['compiler'][0]
        self._binpath = dBin
        # _unicodeOpt
        self._unicodeOpt = self._versionOpt % ('Python_Unicode_UCS' + ((sys.maxunicode == 0xFFFF and '2') or '4'))

    def _initialize(self):
        # It is intended that this method be implemented by subclasses.
        raise NotImplementedError( "Cannot initialize DCompiler, use DMDDCompiler or GDCDCompiler instead.")

    def _def_file(self, output_dir, output_filename):
        """A list of options used to tell the linker how to make a dll/so. In
        DMD, it is the .def file. In GDC, it is
        ['-shared', '-Wl,-soname,blah.so'] or similar."""
        raise NotImplementedError( "Cannot initialize DCompiler, use DMDDCompiler or GDCDCompiler instead.")

    def _lib_file(self, libraries):
        return ''

    def find_library_file(self, dirs, lib, debug=0):
        shared_f = self.library_filename(lib, lib_type='shared')
        static_f = self.library_filename(lib, lib_type='static')

        for dir in dirs:
            shared = os.path.join(dir, shared_f)
            static = os.path.join(dir, static_f)

            if os.path.exists(shared):
                return shared
            elif os.path.exists(static):
                return static

        return None

    def versionOpts(self):
        # Python version option allows extension writer to take advantage of
        # Python/C API features available only in recent version of Python with
        # a version statement like:
        #   version(Python_2_4_Or_Later) {
        #     Py_ConvenientCallOnlyAvailableInPython24AndLater();
        #   } else {
        #     // Do it the hard way...
        #   }
        def pvo(opt):
            optf = 'Python_%d_%d_Or_Later'
            def pv2(minor):
                ret = []
                if not self.build_exe:
                    ret.append(opt % 'PydPythonExtension')
                ret.extend([opt % (optf % (2,m)) for m in range(4,minor+1)])
                return ret
            def pv3(minor):
                return [opt % (optf % (3,m)) for m in range(0,minor+1)]
            major = sys.version_info[0]
            minor = sys.version_info[1]
            if major == 2: return pv2(minor)
            if major == 3: return  pv2(7) + pv3(minor)
            assert False, "what python version is this, anyways?"
        return pvo(self._versionOpt) + [self._unicodeOpt]

    def compile(self, sources,
        output_dir=None, macros=None, include_dirs=None, debug=0,
        extra_preargs=None, extra_postargs=None, depends=None
    ):
        macros = macros or []
        include_dirs = include_dirs or []
        extra_preargs = extra_preargs or []
        extra_postargs = extra_postargs or []
            
        pythonVersionOpts = self.versionOpts()

        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        binpath = _qp(self._binpath)
        compileOpts = self._compileOpts
        outputOpts = self._outputOpts

        includePathOpts = []

        # All object files will be placed in one of three directories:
        # infra   - All of the infrastructure's object files.
        # project - The project's own object files.
        # outside - Any source files specified by the project which are not
        #           contained in the project's own directory.
        orig_sources = sources
        sources = []
        for source in orig_sources:
            if os.path.abspath(source).startswith(os.getcwd()):
                sources.append((winpath(source,self.winonly), 'project'))
            else:
                sources.append((winpath(source, self.winonly), 'outside'))

        if self.with_pyd:
            for file in _pydFiles:
                filePath = os.path.join(_infraDir, 'pyd', file)
                if not os.path.isfile(filePath):
                    raise DistutilsPlatformError("Required Pyd source file '%s' is"
                        " missing." % filePath
                    )
                sources.append((winpath(filePath,self.winonly), 'infra'))
            for file in _utilFiles:
                filePath = os.path.join(_infraDir, 'util', file)
                if not os.path.isfile(filePath):
                    raise DistutilsPlatformError("Required util source file '%s' is"
                        " missing." % filePath
                    )
                sources.append((winpath(filePath,self.winonly), 'infra'))
            for file in _metaFiles:
                filePath = os.path.join(_infraDir, 'meta', file)
                if not os.path.isfile(filePath):
                    raise DistutilsPlatformError("Required meta source file"
                        " '%s' is missing." % filePath
                    )
                sources.append((winpath(filePath,self.winonly), 'infra'))
        if self.build_deimos:
            for file in _deimosFiles:
                filePath = os.path.join(_infraDir, 'deimos', 'python', file)
                if not os.path.isfile(filePath):
                    raise DistutilsPlatformError("Required deimos header "
                        "file '%s' is missing." % filePath
                    )
                sources.append((winpath(filePath,self.winonly), 'infra'))
        # If using PydMain, parse the template file
        if self.build_exe:
            pass
        elif self.with_main:
            name = self.proj_name
            # Store the finished pydmain.d file alongside the object files
            infra_output_dir = winpath(os.path.join(output_dir, 'infra'), self.winonly)
            if not os.path.exists(infra_output_dir):
                os.makedirs(infra_output_dir)
            mainFilename = os.path.join(infra_output_dir, 'pydmain.d')
            make_pydmain(mainFilename, name)
            sources.append((winpath(mainFilename,self.winonly), 'infra'))
        # Add the infraDir to the include path for pyd, meta, and utils.
        includePathOpts += self._includeOpts
        includePathOpts[-1] = includePathOpts[-1] % winpath(os.path.join(_infraDir), self.winonly)
        
        if self.build_exe:
            pass
        else:
            # Add DLL/SO boilerplate code file.
            if _isPlatWin:
                boilerplatePath = os.path.join(_infraDir, 'd',
                    'python_dll_windows_boilerplate.d'
                )
            else:
                boilerplatePath = os.path.join(_infraDir, 'd',
                    'python_so_linux_boilerplate.d'
                )
            if not os.path.isfile(boilerplatePath):
                raise DistutilsFileError('Required supporting code file "%s"'
                    ' is missing.' % boilerplatePath
                )
            sources.append((winpath(boilerplatePath,self.winonly), 'infra'))

        userVersionAndDebugOpts = (
              [self._versionOpt % v for v in self.versionFlagsFromExt] +
              [self._debugOpt   % v for v in self.debugFlagsFromExt]
        )

        # Optimization opts
        if debug:
            optimizationOpts = self._debugOptimizeOpts
        elif self.optimize:
            optimizationOpts = self._releaseOptimizeOpts
        else:
            optimizationOpts = self._defaultOptimizeOpts

        print ('sources: %s' % ([os.path.basename(s) for s, t in sources],))

        objFiles = []
        for source, source_type in sources:
            outOpts = outputOpts[:]
            objFilename = cygpath(os.path.splitext(source)[0],self.winonly) + self.obj_extension
            if source_type == 'project':
                objName = os.path.join(output_dir, 'project', objFilename)
            elif source_type == 'outside':
                objName = os.path.join(output_dir, 'outside', os.path.basename(objFilename))
            else: # infra
                objName = os.path.join(output_dir, 'infra', os.path.basename(objFilename))
            if not os.path.exists(os.path.dirname(objName)):
                os.makedirs(os.path.dirname(objName))
            objFiles.append(winpath(objName,self.winonly))
            outOpts[-1] = outOpts[-1] % _qp(winpath(objName,self.winonly))
            cmdElements = (
                [binpath] + extra_preargs + compileOpts +
                pythonVersionOpts + optimizationOpts +
                includePathOpts + outOpts + userVersionAndDebugOpts +
                [_qp(source)] + extra_postargs
            )
            cmdElements = [el for el in cmdElements if el]
            spawn0(self,cmdElements)
        return objFiles

    def link (self,
        target_desc, objects, output_filename,
        output_dir=None,
        libraries=None, library_dirs=None, runtime_library_dirs=None,
        export_symbols=None, debug=0,
        extra_preargs=None, extra_postargs=None,
        build_temp=None, target_lang=None
    ):
        # Distutils defaults to None for "unspecified option list"; we want
        # empty lists in that case (this substitution is done here in the body
        # rather than by changing the default parameters in case distutils
        # passes None explicitly).
        libraries = libraries or []
        library_dirs = library_dirs or []
        runtime_library_dirs = runtime_library_dirs or []
        export_symbols = export_symbols or []
        extra_preargs = extra_preargs or []
        extra_postargs = extra_postargs or []

        binpath = self._binpath
        if hasattr(self, '_linkOutputOpts'):
            outputOpts = self._linkOutputOpts[:]
        else:
            outputOpts = self._outputOpts[:]
        objectOpts = [_qp(fn) for fn in objects]

        (objects, output_dir) = self._fix_object_args (objects, output_dir)
        (libraries, library_dirs, runtime_library_dirs) = \
            self._fix_lib_args (libraries, library_dirs, runtime_library_dirs)
        if runtime_library_dirs:
            self.warn('This CCompiler implementation does nothing with'
                ' "runtime_library_dirs": ' + str(runtime_library_dirs)
            )

        if output_dir and os.path.basename(output_filename) == output_filename:
            output_filename = os.path.join(output_dir, output_filename)
        else:
            if not output_filename:
                raise DistutilsFileError( 'Neither output_dir nor' \
                    ' output_filename was specified.')
            output_dir = os.path.dirname(output_filename)
            if not output_dir:
                raise DistutilsFileError( 'Unable to guess output_dir on the'\
                    ' bases of output_filename "%s" alone.' % output_filename)

        # Format the output filename option
        # (-offilename in DMD, -o filename in GDC)
        outputOpts[-1] = outputOpts[-1] % _qp(winpath(output_filename,self.winonly))

        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        if not self._need_link(objects, output_filename):
            print ("All binary output files are up to date.")
            return

        if self.build_exe:
            sharedOpts = []
            pythonLibOpt = []
            if target_desc != cc.CCompiler.EXECUTABLE:
                raise LinkError('This CCompiler implementation should be building'
                    ' an executable'
                )
        else:
            # The .def file (on Windows) or -shared and -soname (on Linux)
            sharedOpts = self._def_file(build_temp, output_filename)
            if target_desc != cc.CCompiler.SHARED_OBJECT:
                raise LinkError('This CCompiler implementation should be building '
                    ' a shared object'
                )
        # The python .lib file, if needed
        pythonLibOpt = self._lib_file(libraries)
        if pythonLibOpt:
            pythonLibOpt = _qp(pythonLibOpt)


        # Library linkage options
        print ("library_dirs: %s" % (library_dirs,))
        print ("runtime_library_dirs: %s" % (runtime_library_dirs,))
        print ("libraries: %s"% (libraries,))
        libOpts = gen_lib_options(self, library_dirs, runtime_library_dirs, libraries)

        # Optimization opts
        if debug:
            optimizationOpts = self._debugOptimizeOpts
        elif self.optimize:
            optimizationOpts = self._releaseOptimizeOpts
        else:
            optimizationOpts = self._defaultOptimizeOpts

        cmdElements = (
            [binpath] + extra_preargs + self._linkOpts + optimizationOpts +
            outputOpts + [pythonLibOpt] + objectOpts + libOpts + sharedOpts +
            extra_postargs
        )
        cmdElements = [el for el in cmdElements if el]

        spawn0(self,cmdElements)

class DMDDCompiler(DCompiler):
    compiler_type = 'dmd'

    executables = {
        'preprocessor' : None,
        'compiler'     : ['dmd'],
        'compiler_so'  : ['dmd'],
        'linker_so'    : ['dmd'],
        'linker_exe'   : ['dmd'],
    }

    _env_var = 'DMD_BIN'

    def _initialize(self):
        self.winonly = True
        # _compileOpts
        self._compileOpts = ['-c']
        # _outputOpts
        self._outputOpts = ['-of%s']
        # _linkOpts
        self._linkOpts = []
        # _includeOpts
        self._includeOpts = ['-I%s']
        # _versionOpt
        self._versionOpt = '-version=%s'
        # _debugOpt
        self._debugOpt = '-debug=%s'
        # _defaultOptimizeOpts
        self._defaultOptimizeOpts = ['-debug']
        # _debugOptimizeOpts
        self._debugOptimizeOpts = self._defaultOptimizeOpts + ['-unittest', '-g']
        # _releaseOptimizeOpts
        self._releaseOptimizeOpts = ['-version=Optimized', '-release', '-O', '-inline']
        # StackThreads support
        self._st_support = True

    #def link_opts(self, 

    def _def_file(self, output_dir, output_filename):
        if _isPlatWin:
            # Automatically create a .def file:
            defFilePath = os.path.join(output_dir, 'infra', 'python_dll_def.def')
            make_pyddef(
                defFilePath,
                os.path.basename(output_filename)
            )
            return [winpath(defFilePath,self.winonly)]
        else:
            return []

    def _lib_file(self, libraries):
        if _isPlatWin:
            # The DMD-compatible .lib file can be generated with implib.exe
            # (from the Digital Mars "Basic Utilities" package) using a command
            # series similar to the following:
            #   cd C:\Windows\system32
            #   \path\to\dm\bin\implib.exe /system python24_digitalmars.lib python24.dll
            #
            # I chose not to incorporate automatic .lib generation into this
            # code because Python X.Y releases are fairly infrequent, so it's
            # more convenient to distribute a pre-extracted .lib file to the
            # users and spare them the need for the "Basic Utilities" package.
            pythonDMDLibPath = _qp(os.path.join(_infraDir, 'python',
                'python%s_digitalmars.lib' % _pyVerXY
            ))
            if not os.path.isfile(pythonDMDLibPath):
                raise DistutilsFileError('The DMD-compatible Python .lib file'
                    ' which should be located at "%s" is missing.  Try'
                    ' downloading a more recent version of celeriD that'
                    ' contains a .lib file appropriate for your Python version.'
                    % pythonDMDLibPath
                )
            pythonLibOpt = _qp(winpath(pythonDMDLibPath,self.winonly))

            # distutils will normally request that the library 'pythonXY' be
            # linked against.  Since D requires a different .lib file from the
            # one used by the C compiler that built Python, and we've just
            # dealt with that requirement, we take the liberty of removing the
            # distutils-requested pythonXY.lib.
            if 'python' + _pyVerXY in libraries:
                libraries.remove('python' + _pyVerXY)
            if False and _isPlatCygwin and 'python' + _pyVerXDotY  in libraries:
                libraries.remove('python' + _pyVerXDotY)
            return pythonLibOpt
        else:
            return ''

    def library_dir_option(self, dir):
        self.warn("Don't know how to set library search path for DMD.")
        #raise DistutilsPlatformError, "Don't know how to set library search path for DMD."

    def runtime_library_dir_option(self, dir):
        self.warn("Don't know how to set runtime library search path for DMD.")
        #raise DistutilsPlayformError, "Don't know how to set runtime library search path for DMD."

    def library_option(self, lib):
        if _isPlatWin:
            return self.library_filename(lib)
        else:
            return '-L-l' + lib

class GDCDCompiler(DCompiler):
    compiler_type = 'gdc'

    executables = {
        'preprocessor' : None,
        'compiler'     : ['gdc'],
        'compiler_so'  : ['gdc'],
        'linker_so'    : ['gdc'],
        'linker_exe'   : ['gdc'],
    }

    _env_var = 'GDC_BIN'

    def _initialize(self):
        # _compileOpts
        self._compileOpts = ['-fPIC', '-c']
        # _outputOpts
        self._outputOpts = ['-o', '%s']
        # _linkOpts
        self._linkOpts = ['-fPIC', '-nostartfiles', '-shared']
        # _includeOpts
        self._includeOpts = ['-I', '%s']
        # _versionOpt
        self._versionOpt = '-fversion=%s'
        # _debugOpt
        self._debugOpt = '-fdebug=%s'
        # _defaultOptimizeOpts
        self._defaultOptimizeOpts = ['-fdebug']
        # _debugOptimizeOpts
        self._debugOptimizeOpts = self._defaultOptimizeOpts + ['-g', '-funittest']
        # _releaseOptimizeOpts
        self._releaseOptimizeOpts = ['-fversion=Optimized', '-frelease', '-O3', '-finline-functions']
        # StackThreads support
        self._st_support = False

    def _def_file(self, output_dir, output_filename):
        return ['-Wl,-soname,' + os.path.basename(output_filename)]

    def library_dir_option(self, dir):
        return '-L' + dir

    def runtime_library_dir_option(self, dir):
        return '-Wl,-R' + dir

    def library_option(self, lib):
        return '-l' + lib

class LDCDCompiler(DCompiler):
    compiler_type = 'ldc'
    linker_type = 'gcc'

    executables = {
        'preprocessor' : None,
        'compiler'     : ['ldc2'],
        'compiler_so'  : ['ldc2'],
        'linker_so'    : ['gcc'],
        'linker_exe'   : ['gcc'],
    }

    # this is not a env! (but it isn't GDC)
    _env_var = 'LDC_BIN'

    def _initialize(self):
        # _compileOpts
        self._compileOpts = ['-relocation-model=pic', '-c']
        # _outputOpts
        self._outputOpts = ['-of', '%s']
        self._linkOutputOpts = ['-o', '%s']
        # _linkOpts
        self._linkOpts = ['-nostartfiles', '-shared','-Wl,--no-as-needed','-lphobos-ldc','-ldruntime-ldc', '-lrt','-lpthread','-ldl','-lm']
        # _includeOpts
        self._includeOpts = ['-I', '%s']
        # _versionOpt
        self._versionOpt = '-d-version=%s'
        # _debugOpt
        self._debugOpt = '-d-debug=%s'
        # _defaultOptimizeOpts
        self._defaultOptimizeOpts = ['-d-debug']
        # _debugOptimizeOpts
        self._debugOptimizeOpts = self._defaultOptimizeOpts + ['-g', '-unittest']
        # _releaseOptimizeOpts
        self._releaseOptimizeOpts = ['-fversion=Optimized', '-release', '-O3', '-finline-functions']
        # StackThreads support
        self._st_support = False

    def _def_file(self, output_dir, output_filename):
        return ['-Wl,-soname,' + os.path.basename(output_filename)]

    def library_dir_option(self, dir):
        return '-L' + dir

    def runtime_library_dir_option(self, dir):
        return '-Wl,-R' + dir

    def library_option(self, lib):
        return '-l' + lib
    def link (self, *args, **kwargs):
        target_desc = args[0]
        if target_desc != cc.CCompiler.SHARED_OBJECT:
            raise LinkError('This CCompiler implementation does not know'
                ' how to link anything except an extension module (that is, a'
                ' shared object file).'
            )
        self._binpath = self.executables['linker_so'][0]
        return DCompiler.link(self, *args, **kwargs)
        

# Utility functions:
def _findInPath(fileName, startIn=None):
    # Find a file named fileName in the PATH, starting in startIn.
    try:
        path = os.environ['PATH']
    except KeyError:
        pass
    else:
        pathDirs = path.split(os.pathsep)
        if startIn:
            if startIn in pathDirs:
                pathDirs.remove(startIn)
            pathDirs.insert(0, startIn)

        for pd in pathDirs:
            tentativePath = os.path.join(pd, fileName)
            if os.path.isfile(tentativePath):
                return tentativePath

    return None


def _qp(path): # If path contains any whitespace, quote it.
    if len(path.split()) == 1:
        return path
    else:
        return '"%s"' % path

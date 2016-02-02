from __future__ import print_function
from distutils import sysconfig
import sys
import getopt

help = '''Creates a dub.json for a project depending
on pyd.

Usage:
Run with the python interpreter you want to build for.
Pass the desired name of the package as the first arg
and optionally pass "embed" if you want to embed
python in D instead of the default, which is to build
a extension module for python.

E.g.

"python ./genDubConfig.py myModuleName embed"

The output is printed to stdout, so you may want to
pipe it straight to a file e.g.

"python ./genDubConfig.py myModuleName > dub.json"
'''

if len(sys.argv) < 2 or (len(sys.argv) > 2 and sys.argv[2] != 'embed'):
    print(help, file=sys.stderr)
    exit(1)

name = sys.argv[1]
embed = len(sys.argv) > 2 
vinfo = sys.version_info
py2 = vinfo[0] == 2
py3 = vinfo[0] == 3
libdir = sysconfig.get_config_var("LIBDIR")
libname = "python" + sysconfig.get_config_var('VERSION') + (sys.abiflags if py3 else '')

print(
'''{
    "name": "''' + name + '''",
    "sourcePaths": ["."],
    "targetType": ''' + ('''"executable"''' if embed else '''"dynamicLibrary"''') + ''',
    "targetName": "''' + name + '''",
''' + ('' if embed else '''
    "versions": ["PydPythonExtension"],''') + '''
    "dependencies": { "pyd": { "path": "../../" } },
    "subConfigurations": { "pyd": "python''' + str(vinfo[0]) + str(vinfo[1]) + '''" },
    "lflags-posix": ["-L''' + libdir + '''"],
    "libs": ["''' + libname + '''"]''' + ('' if embed else ''',
    "postBuildCommands-posix": [
        "mv lib$$DUB_TARGET_NAME.so $$DUB_TARGET_NAME.so"
    ]''') + '''
}''')

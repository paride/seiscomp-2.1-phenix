from distutils.core import setup, Extension

setup(name = 'seiscomp',
    packages = ['seiscomp', 'seiscomp.cfgdb', 'seiscomp.qc'],
    ext_modules = [
        Extension('seiscomp.plugin', 
            sources = ['seiscomp/plugin/pyxplugin.c'],
            include_dirs = ['../libslplugin'],
            library_dirs = ['../libslplugin'],
            libraries = ['slplugin']
        )
    ]
)


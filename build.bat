IF NOT EXIST build mkdir build
pushd build

set pp_disabled_warnings=%comment_for_cleanup% /wd4820 /wd4201 /wd4996 /wd4127 /wd4061 /wd4514 /wd4189 /wd4100 /wd4668
set pp_compiler_flags=/Od /Zi /Wall /nologo
set pp_linker_flags=/INCREMENTAL:NO

cl %pp_compiler_flags% %pp_disabled_warnings% %pp_linker_flags% p:\code\minicc\main.cc p:\code\minicc\atom.cc

popd build

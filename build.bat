IF NOT EXIST build mkdir build
pushd build

set disabled_warnings=%comment_for_cleanup% /wd4820 /wd4201 /wd4996 /wd4127 /wd4061 /wd4514 /wd4189 /wd4100 /wd4668
set compiler_flags=/Od /Zi /Wall /nologo
set linker_flags=/INCREMENTAL:NO
set debug=/DDEBUG

cl %compiler_flags% %disabled_warnings% %linker_flags% %debug%^
   p:\code\minicc\main.cc^
   p:\code\minicc\atom.cc^
   p:\code\minicc\lexer.cc^
   p:\code\minicc\file.cc

popd build

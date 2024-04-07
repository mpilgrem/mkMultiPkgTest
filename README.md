# mkMultiPkgTest

A Haskell project to create an executable to help create a multi-package Stack
project.

Use the executable as follows (for example):
~~~text
mkMultiPkgTest noOpTest 80
cd noOpTest
stack --snapshot lts-19.33 init
stack build
~~~

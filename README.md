webaudio-examples
=================

This project illustrates a few simple examples of wabaudio usage in PureScript.  Examples are taken from the O'Reilly book [Web Audio API](http://chimera.labs.oreilly.com/books/1234000001552/) by Boris Smus.  His JavaScript examples are [here](https://github.com/borismus/webaudioapi.com) on GitHub and can be tried out on his [samples](http://webaudioapi.com/samples/) page.

The aim of the project is firstly to teach myself more web-audio and secondly to try to assess the usability of the purescript-webaudio API. The intention is to factor any missing features back in to my fork of the API.

Building
--------

from the current directory:

    $ bower install
    $ ./build.sh
    
Then host dist/webaudio-examples.html on your web server of choice.
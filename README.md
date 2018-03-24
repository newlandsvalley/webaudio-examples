webaudio-examples
=================

This project illustrates a few basic examples of webaudio usage in PureScript.  Examples are inspired in the most part by the O'Reilly book [Web Audio API](http://chimera.labs.oreilly.com/books/1234000001552/) by Boris Smus, although at the moment I am not keen to write any UI for them.  His JavaScript examples are [here](https://github.com/borismus/webaudioapi.com) on GitHub and can be tried out on his [samples](http://webaudioapi.com/samples/) page.

Other examples are based on blog posts by [Chris Lowis](http://blog.chrislowis.co.uk/) and [Output Channel](http://outputchannel.com/).

Most of the examples can now also be expressed declaratively (and hence more simply) using [purescript-audiograph](https://github.com/newlandsvalley/purescript-audiograph).

The aim of the project is firstly to teach myself more web-audio and secondly to try to assess the expressivity of the purescript-webaudio API. The intention is to factor any missing features back in to my fork of the API.

Completed so far
----------------

* Rhythm
* Volume
* Filter
* Oscillator
* Crossfade
* Feedback
* FrequencyModulation
* Cowbell

Building
--------

from the current directory:

    $ bower install
    $ ./build.sh

Then host dist/webaudio-examples.html on your web server of choice.

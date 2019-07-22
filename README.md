# purescript-codec-argonaut

[![Latest release](http://img.shields.io/github/release/garyb/purescript-codec-argonaut.svg)](https://github.com/garyb/purescript-codec-argonaut/releases)
[![Build Status](https://travis-ci.org/garyb/purescript-codec-argonaut.svg?branch=master)](https://travis-ci.org/garyb/purescript-codec-argonaut)

Bi-directional codecs for [argonaut](https://github.com/purescript-contrib/purescript-argonaut-core).

This library is build on `purescript-codec` and offers a different approach to dealing with JSON encoding/decoding than `purescript-argonaut-codecs`. Instead of using type classes, codecs are constructed as values explicitly. As long as the basic codec values provided by this library are used, the codecs are guaranteed to roundtrip succesfully.

For more information on the motivation behind this library, I [wrote a bit about my problems with typeclass codecs](http://code.slipthrough.net/2018/03/13/thoughts-on-typeclass-codecs/) previously.

## Installation

```
bower install purescript-codec-argonaut
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-codec-argonaut).

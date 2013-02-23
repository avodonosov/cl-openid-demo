This program demonstrates how to provide OpenID login in Common Lisp web applications.

See it running at Heroku: http://cl-openid-demo.herokuapp.com/
(with the help of [CL Heroku buildpack](https://github.com/avodonosov/heroku-buildpack-cl2/)).

Powered by [cl-open-id](http://common-lisp.net/project/cl-openid/).

_Temporary Hint: In Quicklisp 2013-02-17 cl-openid is broken. For your development
either get the previous Quicklisp dist by `(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2013-01-28/distinfo.txt" :replace t :prompt nil)` or use the recent cl-openid from Trac._

Author
------
  Anton Vodonosov, avodonosov@yandex.ru

Copying
-------

The code of cl-openid-demo is in public domain.

The directory jquery-openid contains the
[jQuery OpenID Plugin by Jarrett Vance] (http://jvance.com/pages/JQueryOpenIDPlugin.xhtml)
(with slight modification by me). The jQuey OpenID Plugin is under the
[Creative Commons Attribution License](https://creativecommons.org/licenses/by/3.0/).

The lisp mascot is a public domain image [by Contad Barski] (http://www.lisperati.com/logo.html).


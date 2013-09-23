# DEAL
##### Lisp-based tabletop game prototyping and playtesting tool 

## Requirements

Deal directly depends on [optima](https://github.com/m2ym/optima), [cl-ppcre](http://weitz.de/cl-ppcre/), [drakma](http://weitz.de/drakma/), [hunchentoot](http://weitz.de/hunchentoot/), [cl-json](http://common-lisp.net/project/cl-json/) and [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/). All of those can be installed using [`quicklisp`](http://www.quicklisp.org/beta/).

Deal depends on an external SSE publishing service to provide push capability to its clients. The easiest approach is using [nginx](http://wiki.nginx.org/Main) with its [push-stream module](https://github.com/wandenberg/nginx-push-stream-module). The installation instructions deal with this route, but you can probably set up some other streaming service that can be set to publish messages based on input from `localhost` HTTP requests.

## Installation

**1** Pick a directory and run the following in it:

    git clone http://github.com/wandenberg/nginx-push-stream-module.git
    wget http://nginx.org/download/nginx-1.2.0.tar.gz
    tar xzvf nginx-1.2.0.tar.gz
    cd nginx-1.2.0
    ./configure --add-module=../nginx-push-stream-module
    make
    sudo make install

**2** Pick another directory, and clone [this project](https://github.com/Inaimathi/deal) with `git clone https://github.com/Inaimathi/deal.git`

**3** Change the config variables in [`package.lisp`](https://github.com/Inaimathi/deal/blob/master/package.lisp) to your liking

**4** Change the contents of [`nginx-deal`](https://github.com/Inaimathi/deal/blob/master/nginx-deal) to your liking. In particular, by default it binds port `80` for listening, port `9080` for publishing/subscribing, and proxies to port `8080` for any dynamic requests (it should proxy to wherever `hunchentoot` is listening for requests). It also serves static files from `/home/inaimathi/projects/deal`, which is certainly incorrect unless you're me.

**5** Copy `nginx-deal` to your `nginx` config directory, and run `nginx`. If you did this on Debian, the newly installed binary will be at `/usr/local/nginx/sbin/nginx`, and the config file will be at `/usr/local/nginx/conf/nginx.conf`.

**6** Run your copy of `deal` by starting a lisp in the appropriate directory and evaluating `(ql:quickload :deal-ui)`

That's a *lot* more compliacted than I like to make installation, but there isn't a good option for async serving natively in Common Lisp at the moment. Hopefully I can fix that in a future release. 

In any case, you should now be able to visit `[your-server]/static/index.html` and play.

## License

This program is released under the GNU AGPL (License text can be found in the [LICENSE.md](https://github.com/Inaimathi/deal/blob/master/LICENSE.md) file, or at <http://www.gnu.org/licenses/agpl-3.0.html>)

Deal bundles some javascript libraries.

- Minified copies of [jQuery](http://jquery.com/) and [jQueryUI](http://jqueryui.com/); both are dual-licensed under the [GPL and Expat licenses](http://jquery.org/license/). Readable source can be found at respective project pages.
- [FileSaver.js](https://github.com/eligrey/FileSaver.js) and [Blob.js](https://github.com/eligrey/Blob.js) for JS-based file saving; both dual-licensed under [Expat and X11 licenses](https://github.com/eligrey/FileSaver.js/blob/master/LICENSE.md)
- A modified version of [jquery.ui.rotatable](http://vremenno.net/js/jquery-ui-rotation-using-css-transform/) duel-licensed under the [Expat](http://opensource.org/licenses/MIT) and [GPL](http://www.gnu.org/licenses/gpl.html) licenses

Deal bundles some images from [OpenGameArt.org](http://opengameart.org/). This includes

- [chess/go/checkers piees](http://opengameart.org/content/boardgame-tiles) ([CC-BY 3.0 license](http://creativecommons.org/licenses/by/3.0/) by [Sharm](http://opengameart.org/users/sharm), aka Lanea Zimmerman)
- [plants](http://opengameart.org/content/lpc-plant-repack) (Dual-licensed under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/) and [GPL3](http://www.gnu.org/licenses/gpl-3.0.html), re-packaged by [William. Thompsonj](http://opengameart.org/users/williamthompsonj))

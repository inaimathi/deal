# DEAL
##### Lisp-based tabletop game prototyping and playtesting tool 

## Requirements

Deal directly depends on a fuckton of other Lisp libraries, all of which will be installed automatically by [`quicklisp`](http://www.quicklisp.org/beta/) the first time you run it. If you plan on installing Deal manually, the required libraries are

    :alexandria :anaphora :bordeaux-threads :cl-base64 :cl-css :cl-fad :cl-json
    :cl-ppcre :cl-who :deal :flexi-streams :ironclad :optima :parenscript
    :trivial-timeout :usocket

## Installation

**1** Pick a directory, and clone [this project](https://github.com/Inaimathi/deal) with `git clone https://github.com/Inaimathi/deal.git`

**2** Start a lisp in that directory and evaluate `(ql:quickload :deal-ui)`

**3** Evaluate `(deal::start 8080)`

You should now be able to visit `[your-server]:8080/` and play.

**Optionally**

If you're going to be hosting a public server, it's a good idea to set up a reverse proxy, and leave the static file handling up to something that can do it more efficiently than House. I recommend [nginx](http://nginx.org/). To make that easier, I've included [a config file](https://github.com/Inaimathi/deal/blob/master/nginx-deal) that you can drop into `/etc/nginx/sites-enabled/` if you're on Debian.

## TODO

- Look into iolib/socket as an alternative to the shitty implementation of `read-byte-no-hang`
- Sessions should actually expire at some point
- Add session-expiration hooks

## License

This program is released under the GNU AGPL (License text can be found in the [LICENSE.md](https://github.com/Inaimathi/deal/blob/master/LICENSE.md) file, or at <http://www.gnu.org/licenses/agpl-3.0.html>). Unofficial, legally non-binding short version: *You are free to use, distribute, modify and distribute modified copies as long as you give each of your users (including those that just connect to your Deal server as web clients) access to your (potentially modified) source code*.

Deal bundles some javascript libraries.

- Minified copies of [jQuery](http://jquery.com/) and [jQueryUI](http://jqueryui.com/); both are dual-licensed under the [GPL and Expat licenses](http://jquery.org/license/). Readable source can be found at respective project pages.
- [FileSaver.js](https://github.com/eligrey/FileSaver.js) and [Blob.js](https://github.com/eligrey/Blob.js) for JS-based file saving; both dual-licensed under [Expat and X11 licenses](https://github.com/eligrey/FileSaver.js/blob/master/LICENSE.md)
- A modified version of [jquery.ui.rotatable](http://vremenno.net/js/jquery-ui-rotation-using-css-transform/) duel-licensed under the [Expat](http://opensource.org/licenses/MIT) and [GPL](http://www.gnu.org/licenses/gpl.html) licenses

Deal bundles some images from [OpenGameArt.org](http://opengameart.org/). This includes

- [chess/go/checkers piees](http://opengameart.org/content/boardgame-tiles) ([CC-BY 3.0 license](http://creativecommons.org/licenses/by/3.0/) by [Sharm](http://opengameart.org/users/sharm), aka Lanea Zimmerman)
- [plants](http://opengameart.org/content/lpc-plant-repack) (Dual-licensed under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/) and [GPL3](http://www.gnu.org/licenses/gpl-3.0.html), re-packaged by [William. Thompsonj](http://opengameart.org/users/williamthompsonj))

Deal bundles an image from another source.

- [sticky-note icon](https://www.iconfinder.com/icons/43879/note_icon#size=32) from [Momentum Labs](http://momentumdesignlab.com/) released under [CC-BY 3.0](http://creativecommons.org/licenses/by/3.0/).

# README

httprepl.el allows you to interactively make HTTP requests at a
REPL-like UI within Emacs.

## Installation

Firstly, httprepl requires the following dependencies:

* https://github.com/magnars/dash.el
* https://github.com/magnars/s.el

You should install these anyway, they make working with elisp much
more comfortable.

Drop this package somewhere on your load-path or

    (add-to-list 'load-path (expand-file-name "/path/to/httprepl.el/"))

Then

    (require 'httprepl)

To get started, run <kbd>M-x httprepl</kbd>

Check out <kbd>M-x customize-group httprepl</kbd> for configuration.

This has been tested on Emacs 24.3 on Linux and Mac OS X.

## Features

* Make HTTP requests using Emacs built in url package. This should
  work for environments without curl.
* Make HTTP requests using curl.
* Supports GET, POST, PUT, DELETE, OPTIONS, HEAD, PATCH, TRACE and
  CONNECT requests.
* Supports adding extra headers.
* Supports adding inline request bodies.
* Supports opening responses in a new buffer using C-c C-c. Based on
  the content-type of the response a sequence of functions can be
  applied to this buffer. This allows for syntax-highlighting,
  pretty-printing, whatever really.

## Screenshots

The screenshot shows a sample REPL session. A PUT request with an
extra header and a body is executed. The response is then opened in a
buffer. As you can see, the content type is detected and the buffer is
put in to js-mode with the response headers commented out.

![httprepl](http://www.gregsexton.org/images/httprepl.png)

## License

MIT license

This utility takes a plain text description of relational database schema
(including tables, their attributes and integrity constraints) and produces
a visual diagram modeling the description. The visualization is produced 
using GraphViz; `morela` can output graphs in a variety of formats, 
including but not limited to: pdf, svg, eps, png, jpg, plain text and dot.

This project was inspired by (and parts of the code are based on) the
[`erd`](http://github.com/BurntSushi/erd) tool by Andrew Gallant, which
draws entity-relationship diagrams based on a plain text description.

At present `morela` is still in early development phase and highly unstable.

### Installation

`morela` requires [GraphViz](http://www.graphviz.org/download/).

#### Stack

Install the [Stack](http://docs.haskellstack.org/en/stable/README/) build tool,
and build from source:

    git clone git://github.com/kukimik/morela
    cd morela
    stack install

`stack install` will put the binary into Stack's standard binary
installation path.  Unless you've overridden it, that's `~/.local/bin`
on Unix and OS X, `%APPDATA%\local\bin` on Windows.
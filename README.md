# Multi-Timer
> :alarm_clock: Time all the things!


A simple way to set up and run multiple timers. Created as an exercise, while learning the [Elm programming language](http://elm-lang.org/).  

**Current stable version:** http://joelchelliah.github.io/multi-timer/

#### Build
Target files can be auto-compiled by Atom whenever saving `.elm` or `.scss` files.

To build them manually run:
```bash
elm-make src/Main.elm --output target/main.js

node-sass --output-style compressed css/style.scss > css/style.min.css
```

#### Run
Running with `elm-reactor` does not support the current css set-up :disappointed:  

```bash
open index.html
```

#### Deploy
Run the `elm-deploy` script!

This will push the compiled `css/style.min.css` and `target/main.js` files to the **gh-pages** branch.


#### Todo
- Simplify timers by removing their wrapper model.
- Turn this into a desktop app using Electron! :rocket:

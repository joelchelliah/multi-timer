# Multi-Timer
> Time all the things!


A simple way to create and run multiple timers. Created as an exercise to learning the [Elm programming language](http://elm-lang.org/).  

**Current stable version:** http://joelchelliah.github.io/multi-timer/

#### Build
Compile `css/style.min.css` and `target/main.js`.  

These files are auto-compiled by atom whenever saving `.elm` or `.scss` files. To build them manually run:
```bash
elm-make src/Main.elm --output target/main.js

node-sass --output-style compressed css/style.scss > css/style.min.css
```


#### Deploy
Push `css/style.min.css` and `target/main.js` to the **gh-pages** branch.  

This process is automated in the `elm-deploy` script!

##### Todo
- Support for adding and removing timers.
- Turn this into a desktop app using Electron! :rocket:

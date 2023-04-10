### Demo

A live hosted version of this code can be [found here](https://dcascaval.github.io/lineage/). Everything runs client-side.

### Documentation 

Full documentation of the language and constructs can be found [here](language-docs.md).

### Setup

This project uses Scala 3.2.1 and Scala JS. We recommend installing with [Coursier](https://get-coursier.io/), which will also install SBT. You will also need NPM globally installed. To run the project, from the console:

```
sbt
> ~fastOptJS/webpack           // Compiles to JS and watches for changes
```

Then, open `index.html` in any browser. You will have to reload when changing scala sources. To reload automatically when sources change, run `./scripts/serve.sh`, which relies on the [`live-reload`](https://www.npmjs.com/package/live-reload) package, installable via `npm install -g live-reload` or `yarn global add live-reload`. If live-reloading, access the file at `localhost:4000` instead of directly.

### Outline

- `js/src/main/scala/elodin`
  - `/dom` : DOM & SVG interaction library
  - `/opt` : Our CAD System
    - `/dsl` : Parser and AST definitions
    - `main.scala` : Root, demos, tests
    - `editor.scala` : UI & Integration
    - `execute.scala` : Language interpreter
    - `render3d.scala` : Rendering with Three JS & state management
    - `programs.scala` : Example programs for the demos
    - `types.scala` : Language type system and helpers
  - `/three` : Context management for Three JS
  - `usage.scala` : Root file

- `shared/src/main/scala`
  - `/geometry` : geometric types supported in the kernel
  - `/opt`:
    - `kernel.scala` : Geoemtric core
    - `operations.scala` : Language operations with lineage logic
    - `queries.scala` : Querying resolution implementation 
    - `reference.scala` : Lineage data structurs

- `jvm/src/test/scala` : Miscellaneous tests

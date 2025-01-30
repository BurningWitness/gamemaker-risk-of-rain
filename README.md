# gamemaker-risk-of-rain

Tools for unpacking and decompiling the GameMaker data file for
[Risk of Rain 1](https://store.steampowered.com/app/248820/Risk_of_Rain/),
called `data.win` on Windows, `data.ios` on MacOS and `game.unx` on Linux.


## Executables

### Examination

```bash
cabal build examine
cabal run examine -- FILE
```

<sup>(where `FILE` is the `data.win`/`game.ios`/`game.unx` file)</sup>

Outputs data file information to stdout.


### Decompiler

```bash
cabal build decompile
cabal run decompile -- FILE
```

<sup>(where `FILE` is the `data.win`/`game.ios` file)</sup>

Disassembles and decompiles game's source code, outputting
[K&R indented](https://www.kernel.org/doc/html/v4.14/process/coding-style.html)
[GML](https://en.wikipedia.org/wiki/GameMaker#GameMaker_Language) to stdout.
Note that this is only available for Windows and MacOS builds of the game;
there was a period when Hopoo distributed YYC-compiled versions of the game,
but they stopped as it caused
[crashes](https://web.archive.org/web/20141121214456/http://riskofraingame.com/risk-of-rain-patch-v1-2-3/).



## Honorable mentions

Kudos to the Undertale datamining squad for sharing their findings. Without them
decompiling the game would take even longer than it already did and would not be
nearly as successful. Links to Undertale stuff:

- Data file format and 0xF instruction list:
  [tomat.dev/undertale](https://tomat.dev/undertale);

- Detailed 0xE instruction list:
  [github.com/gm-archive/acolyte/wiki](https://github.com/gm-archive/acolyte/wiki/Bytecode).

- Corrections to data file format and 0xF instruction list:
  [github.com/krzys-h/UndertaleModTool/wiki](https://github.com/krzys-h/UndertaleModTool/wiki/Corrections-to-GameMaker-Studio-1.4-data.win-format-and-VM-bytecode,-.yydebug-format-and-debugger-instructions).



## Questions

Q: Can this decompile games other than Risk of Rain 1?

A: I tried like five and all of them seemed to use bytecode 0xE, while
   Risk of Rain 1 uses 0xF (if I had to guess people just don't update because
   updating GameMaker
   [breaks](https://www.reddit.com/r/gamemaker/comments/89sukv/the_latest_update_broke_my_game/)
   the project). There are major structural differences between the two, therefore even the
   naming of the modules in the package is `GameMaker.RiskOfRain.*`. If you want to
   decompile a different game, handroll another setup, you're most certainly not
   using Haskell either way.

<br>

Q: Why is this in a separate package and not inside a game build?

A: Because I feel like it makes for a nice little unit by itself, so even though
   it is practically useless outside of a game build, it can still exist as a separate
   package. Seeing how I've been experimenting with making a game for the past ~~two years~~
   eternity, I'm not even sure I'll make a working videogame ever,
   so perhaps someone can use this to figure out how to unpack a different game.



## Contact information

The only contact channel I have is Github, at least as of the time of writing this.

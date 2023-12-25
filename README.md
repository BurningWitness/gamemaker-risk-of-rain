# gamemaker-risk-of-rain

Unpacking and decompilation of
[Risk of Rain 1](https://store.steampowered.com/app/248820/Risk_of_Rain/) files.
More specifically `data.win` (under Windows) and `game.unx` (under Linux).

This package includes:

  * Extraction of about every single resource the data file has to offer;

  * Decompilation of the game's interpreted code (stored inside the
    `CODE` chunk and available only on Windows; Hopoo stopped running
    YYC-compiled code as it caused
    [crashes](https://riskofrain.fandom.com/wiki/Updates#Patch_v1.2.3)).

    Additionally running `cabal run decompile -- FILE`, where `FILE` is the link to a
    `data.win` file will output game's decompiled source code to stdout.

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
   naming of the modules in the package is `GameMaker.RiskOfRain.*`. So imo if you wanna
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

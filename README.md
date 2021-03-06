# Tandy 64

A intro for the Tandy 1000 HX (and compatible) computers released for [Demosplash 2017][8]

<i>Capture from emulator (fonts don't look good, missing border, and some glitches):</i><br/>
[![Tandy 64 Intro](https://img.youtube.com/vi/M2X7e9KnoLk/0.jpg)](https://www.youtube.com/watch?v=M2X7e9KnoLk)

<i>Recorded real hardware: (he, we don't have a way to capture RGBI output yet... sorry)</i><br/>
[![Tandy 64 Intro](https://img.youtube.com/vi/3YsQJ2ajfpU/0.jpg)](https://www.youtube.com/watch?v=3YsQJ2ajfpU)

(The "Tandy 64 Jr." for the [IBM PCJr. here](https://github.com/c64scene-ar/tandy64/tree/ibm_pcjr))


## Requirements

* Tandy 1000 HX (or compatible)
* Tested with 256Kb RAM, but could fit in 128Kb with some minor changes


## How to compile it

* Install [nasm][0]
* Install [alink][1]
* (Optional) Install [Python 3][2]
* (Optional) Install [DosDox-x][3]
* (Optional) Install [upx][4]


And do:

    $ make


To run it in the DosBox-x emulator, do:

    $ make run


## Resources

The `/res` folder has the music, graphics and fonts in their native formats.
And in the `/tools` folder are the scripts to convert them to a more optimized
format for the Tandy.

To convert the resources to native do:

    $ make res

## Source code structure

The `/src` folder contains the source code and all converted resources.
There are 4 `.asm` files:

*   `main.asm`: setups the stack and calls the pre_intro, intro and post_intro in order
*   `pre_intro.asm`: shows the C64 loading screen
*   `intro.asm`: Everything happens here. The code is somewhat messy. There a few state machines to control and sync the different actions. Some pointers:
    * `plasma_tex_*`: refers to "plasma textures", the ones that appear at the very beginning
    * `plasma_*`: (without the `_tex` suffix): refers to the plasma animation.
    * `new_i08_*`: there are like 3 different interrupt handlers. They are used in at different parts
    * `boy_anim_*`: code related to the guy walking/dancing
    * `text_writter_*`: code related to the "cursor writting text"
    * `letter_state_*`: code related to the PVM letters animation
    * `state_*` / `main_state_*`: "main" state machine code
    * `scroll_*`: scroller code
    * `raster_bars_*`: raster bar effect code
*   `post_intro.asm`: the final C64 screen

## Tools

In `tools` folder you will find:

*   `convert_vgm_to_pvm.py`: Converts VGM music format to PVM (Plays VGM Music) music format
    *   Almost the same as VGM but uses Run-leght encoding. As an example, the music that we use
        takes ~78K in VGM format, and around ~48K in PVM format. It is good to have it in less than 64k (1 segment)
    * A stand-along PVM player could be found here: [pvmplay.asm](https://github.com/ricardoquesada/tandy1000-misc/blob/master/snd/pvmplay.asm)
*   `convert_gfx_to_bios_format.py`: Converts graphics to Tandy format.
*   `convert_anim_to_frames.py`: Quick hack to dump animation frames in tandy format

We also used:

*   [Deflemask][6] for composing the music
*   [plasma_generator.py][5]: to visually build the plasma effects
*   [VChar64][7] for the charset
    *   The charset used is based on one used on a c64 intro?? (couldn't find the original intro)
*   [Tandy 1000 HX BIOS dump](https://github.com/ricardoquesada/tandy1000hx-bios)
    *   In order to learn more about the Tandy 1000 HX we disassembled its BIOS


# Lessons learned

*   Comming soon™


# Questions and others

Do you have questions? Do you want to collaborate with PVM? We're here:

-   [http://pungas.space][9]
-   On IRC. [EFnet][10] . Channel #pvm
-   [Twitter](https://twitter.com/pungas64)
-   [Facebook](https://www.facebook.com/PVM1996/)


[0]: http://nasm.us/
[1]: https://github.com/ricardoquesada/alink
[2]: https://www.python.org/downloads/
[3]: http://dosbox-x.com/
[4]: https://upx.github.io/
[5]: https://github.com/ricardoquesada/c64-misc/blob/master/tools/plasma_generator.py
[6]: http://deflemask.com/
[7]: https://github.com/ricardoquesada/vchar64
[8]: http://www.demosplash.org/
[9]: http://pungas.space
[10]: http://www.efnet.org/

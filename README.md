*If this helps you save time or money for your job, please consider supporting the work involved in here ;)* [![Donate](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=DNEEF8GDX2EV6&currency_code=EUR&source=url)

Open Mesh Effect for Blender
============================

This is an edited version of Blender 2.83 LTS including an [Open Mesh Effects](https://github.com/eliemichel/OpenMeshEffect) based modifier.

![OpenMeshEffect modifier for Blender](doc/openmesheffect/openmesheffect-for-blender.png)

This modifier lets you load an Open Mesh Effects plug-ins and use it as a Blender modifier. Such plug-ins can be provided by third party, or even written by yourself following the [Open Mesh Effects](https://github.com/eliemichel/OpenMeshEffect) standard.

## Disclaimer

This is a **work in progress** and by no mean a finished work. Any feedback is welcome, including bug reports, design proposals, code reviews, etc. You can use the [issues](https://github.com/eliemichel/OpenMeshEffectForBlender/issues) to do so. I will post dev updates on my [twitter feed](https://twitter.com/exppad).

## Building

You can just follow the usual [instructions for building Blender](https://wiki.blender.org/wiki/Building_Blender).

## Usage

Create an object and add an *Open Mesh Effect* modifier to it. In the plug-in path, provide the Open Mesh Effect bundle, like `/path/to/something.ofx` (see [openmesheffects.org](https://openmesheffect.org/Implementations/PluginExamples.html) for examples). This will open the binary and list the available plug-ins.

![OpenMeshEffect modifier](doc/openmesheffect/openmesheffect-create.png)


Then select which effect from the plugin you are interested in. With a valid plug-in selected, some parameters will appear.

You can then stack it with other modifiers, and/or apply it.

## License

Blender as a whole, and hence this branch, is licensed under the GNU Public License, Version 3.
Individual files may have a different, but compatible license.

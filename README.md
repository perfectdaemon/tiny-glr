### Disclaimer
**Sorry, guys, but I left Pascal community and the project will not be supported any longer. At now I'm working on the new engine/framework - TypeScript WebGL Engine which hasn't been named yet. You can check it at https://github.com/perfectdaemon/ts-game**

You can use the code or framework itself by your own risk and so on.

May the Force be with you.

### Introduction

Tiny *potentially crossplatform* (Win, Mac, Linux, Android, iOS) 2D/3D game development framework built on  Free Pascal Compiler.

*Notice: It's Windows only for now (0.1 version)*

### Roadmap

 - [x] Early development
 - [x] Some little game to prove framework' abilities
 - [x] 0.1 version (Core, 2D) 
 - [ ] 0.2 version (Base GUI, base 3D, base sound and physics)
 - [ ] 0.3 version (Crossplatform - Android, iOS, Mac, Linux)
 - [ ] 0.4 version (Complex 3D, external tools and editors support)
 - [ ] 0.5 version (Drop repo and make games on Unity 8) )

### Features

**Core**
* Lightweight, no dependencies on other libraries, all-in-one
* Pack files
* LZO compression support
* Tweener — perform animation on curves
* Action Manager — perform independent or queued actions with/without period and delays on start
* Utility classes — stream, list<T>, pool<T>, dictionary<T>, stack<T>, etc.
* Math library - vectors, matrices, quaternions
* Game Screens — internal framework for easiest menu building

**Graphics**
* OpenGL renderer — no deprecated functions used, no FFP, works on OpenGL 2.0+
* Texture formats — bmp (24/32 bit), tga (24/32 bit)
* Texture atlases — [Cheetah Texture Packer](https://github.com/scriptum/Cheetah-Texture-Packer) support
* Mesh formats — obj, engine-based file format
* 2D Sprites — per vertex control, custom pivot point support, batching, scaling
* 2D Fonts — custom pivot point support, max width support, scaling
* 3D Meshes (early dev)
* GUI — Button, Checkbox, Slider, Label, 9-patch support
* Programmable 2d particles

**Physics**
* Box2D support

### Toolset
* Resource packer (folder -> pack file, file -> lzo file)
* Font generator

### Documentation
[Wiki](https://github.com/perfectdaemon/tiny-glr/wiki)

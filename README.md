```pascal
function Core.GetLatestNews(): UnicodeString;
begin
  Exit(
    'Development has become a little slow at early 2015.'
    'Use it for your own risk.'
    'Contact me if you need support'
    );
end;
```

###Introduction###
Tiny *potentially crossplatform* (Win, Mac, Linux, Android, iOS) game development framework built with (and for) Free Pascal Compiler.

*Notice: It's Windows only for now (0.1 version)*

###Roadmap###
 - [x] Early development
 - [x] Some little game to prove framework' abilities
 - [x] 0.1 version (Core, 2D) 
 - [ ] 0.2 version (Base GUI, base 3D, base sound and physics)
 - [ ] 0.3 version (Crossplatform - Android, iOS, Mac, Linux)
 - [ ] 0.4 version (Complex 3D, external tools and editors support)
 - [ ] 0.5 version (Drop repo and make games on Unity 8) )

###Features###
**Core**
* Lightweight, no dependencies on other libraries, all-in-one
* Pack files
* LZO support
* Tweener
* Action Manager — perform independent or queued actions with/without period and delays on start

**Graphics**
* OpenGL Programmable Pipeline — works on ogl 1.5+, but no FFP for future OpenGL ES backend
* Texture formats — bmp (24/32 bit), tga (24/32 bit)
* Texture atlases — [Cheetah Texture Packer](https://github.com/scriptum/Cheetah-Texture-Packer) support
* Mesh formats — obj, engine-based file format
* 2D Sprites — per vertex control, custom pivot point support, batching, scaling
* 2D Fonts — custom pivot point support, max width support, scaling
* 3D Meshes (early dev)
* GUI — Manager, button, nine-patch support (in dev)
* Programmable 2d particles

**Physics**
* Box2D support

###Toolset###
* Resource packer (folder -> pack file, file -> lzo file)
* Font generator

###Documentation###
See Wiki

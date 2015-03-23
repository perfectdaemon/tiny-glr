```pascal
function TinyGLR.CanUse(): WideString;
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
 - [x] Some little game to prove framework' abilities (see src/demos/lunar lander)
 - [x] 0.1 version (Core, 2D) 
 - [ ] 0.2 version (Base GUI, base 3D, base sound and physics)
 - [ ] 0.3 version (Crossplatform - Android, iOS, Mac, Linux)
 - [ ] 0.4 version (Complex 3D, external tools and editors support)
 - [ ] 0.5 version (Drop repo and make games on Unity 8) )

###Features done###
* Lightweight, no dependencies on other libraries, all-in-one
* OpenGL Programmable Pipeline (works on ogl 1.5+, but no FFP for future OpenGL ES backend)
* Texture formats: bmp (24/32 bit), tga (24/32 bit)
* Pack files
* LZO support
* 2D (Sprites, Bitmap Fonts)
* Texture atlases ([Cheetah](https://github.com/scriptum/Cheetah-Texture-Packer) texture atlases supported)
* Programmable 2d particles
* Box2D support
* GUI: Manager, button, nine-patch support (early alpha)
* Meshes (early alpha)
* 3D formats: obj, engine-based file format

###Toolset done###
* Resource packer
* Font generator

###Tutorials###
[not soon, really]

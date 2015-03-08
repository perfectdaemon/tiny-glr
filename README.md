```pascal
function TinyGLR.CanUse(): WideString;
begin
  Exit(
    'Development suspended at early 2015.'
    'Will development be resumed or not — no one knows'
    'Use it for your own risk'
    );
end;
```

###Introduction###
Tiny *potentially crossplatform* (Win, Mac, Linux, Android, iOS) game development framework built with (and for) Free Pascal Compiler.

*Notice: It's Windows only for now (0.1 version)*

###Current roadmap###
 - [x] Early development
 - [x] Some little game to prove framework' abilities (see src/demos/lunar lander)
 - [x] 0.1 version 
 - [ ] [My Multitetris game](https://github.com/perfectdaemon/glr/tree/master/src/demos/Project%20MutilTetris) port 
 - [ ] 0.2 version 
 - [ ] My [other](https://github.com/perfectdaemon/glr/tree/master/src/demos) games' port 
 - [ ] [Space Sim](https://github.com/perfectdaemon/space-sim) port
 - [ ] ...


###Current features###
* Lightweight, no dependencies on other libraries, all-in-one
* OpenGL Programmable Pipeline (works on ogl 1.5+, but no FFP for future OpenGL ES backend)
* Texture formats: bmp (24/32 bit), tga (24/32 bit)
* Pack files
* LZO support
* 2D (Sprites, Bitmap Fonts)
* Texture atlases ([Cheetah](https://github.com/scriptum/Cheetah-Texture-Packer) texture atlases supported)
* Programmable 2d particles
* Box2D support
* GUI (early alpha)
* Meshes (early alpha)

###Current toolset###
* Resource packer
* Font generator

###In progress###
* 3D base functionality (mesh, skeletal, morph, 3ds, fbx, obj, smd)
* GUI
* Sound and physics engines support
* Linux, Mac OS, Android, iOS

###Tutorials###
[not soon]

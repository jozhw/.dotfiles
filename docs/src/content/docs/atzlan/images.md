---
title: Images
description: Documentation of the scripts related to images.
---

## svg to ico

Solution from [stackoverflow](https://graphicdesign.stackexchange.com/questions/77359/how-to-convert-a-square-svg-to-all-size-ico).

Export your SVG `master.svg` to PNG with Inkscape:

```shell
# Install on Ubuntu
sudo apt-get install inkscape
# Other systems: make sure Inkscape is in your PATH

inkscape -w 16 -h 16 -o 16.png master.svg
inkscape -w 32 -h 32 -o 32.png master.svg
inkscape -w 48 -h 48 -o 48.png master.svg
```

Convert the PNG images to ICO with ImageMagick:

```shell
# Install on Ubuntu
sudo apt-get install imagemagick

convert 16.png 32.png 48.png icon.ico
```

Make sure your ICO contains everything:

```shell
$ identify icon.ico
icon.ico[1] ICO 16x16 16x16+0+0 32-bit sRGB 21.2KB 0.000u 0:00.000
icon.ico[0] ICO 32x32 32x32+0+0 32-bit sRGB 21.2KB 0.000u 0:00.000
icon.ico[0] ICO 48x48 48x48+0+0 32-bit sRGB 21.2KB 0.000u 0:00.000

```

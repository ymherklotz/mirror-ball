# mirror-ball

To use a latitude longitude map when lighting a sphere in the environment, the
reflection vector at every point on the sphere is used to get it's colour. As a
simplification, the sphere is assumed to be a perfect mirror, so that one
reflection vector is enough to get the right colour.

The latitude longitude map was created by taking a photo of a mirror ball and
mapping the spherical coordinates to a rectangle.

![latlong map](/data/urbanEM_latlong.png)

The first step is to calculate the normals at every pixel using the position and
size of the sphere. These can be visualised by setting the RGB to the XYZ of the
normal at the pixel.

![normal map](/data/normal.png)

The reflection vector can then be calculated and visualised in the same way, by
using the following formula: `r = 2 (n . v) n - v`.

![reflection map](/data/reflect.png)

The reflection vector can be converted to spherical coordinates, which can in
turn be used to index into the lat-long map. The colour at the indexed pixel is
then set to the position that has that normal.

![final](/data/final.png)

## Building and run

To compile and run, one has to first download
[stack](https://docs.haskellstack.org/en/stable/README/)

The simplest way to do this is by executing the following command:

```
curl -sSL https://get.haskellstack.org/ | sh
```

Then run setup in this directory:

```
stack setup
```

Finally the executable can be built and run using the following:

```
stack build --exec mirror-ball
```

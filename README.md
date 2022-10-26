# Liikennematto

Liikennematto (_Finnish for a "traffic mat"_) is a prototype traffic simulation with a tiny scale. Inspired by traffic mats that children play with.

![Screenshot](docs/screenshot.png)

_Liikennematto in action! [Live demo](http://apps.butsku.com/liikennematto/)_

## Features

### Simulation

-   a two-lane road network
-   collision prevention
-   signal intersections (traffic lights)
-   yield sign based intersections
-   dead end streets
-   prodedural generation of lots and buildings
-   debug tools
    -   pause simulation
    -   spawn cars
    -   road network visualization
    -   road network DOT string export
    -   car debug layers

### Map editor

Draw roads of all kind with the smart editor

-   **left click / tap** to place a piece of road
-   **right click / long tap** to remove road from the tile

#### Restrictions

-   tiles cannot be placed everywhere. In order to avoid awkward road layouts, the editor restricts complexity
-   lots are only placed next to road tiles that have enough distance to intersections, curves and dead ends

## Demo & more information

> Liikennematto works best on a large screen with landscape orientation (iPad, laptops, desktops)

[Live demo (stable)](http://apps.butsku.com/liikennematto/)

[Preview version (often in sync with the master branch)](http://apps.butsku.com/liikennematto/next/)

---

Read more about the project from [Liikennematto dev blog #1: prototyping traffic simulation with Elm](https://matiasklemola.com/liikennematto-dev-blog-one)

[Part #2](https://matiasklemola.com/liikennematto-devlog-two), [Part #3](https://matiasklemola.com/liikennematto-devlog-three), [Part #4](https://matiasklemola.com/liikennematto-devlog-four)

---

[Follow me on Twitter](https://twitter.com/MatiasKlemola) for updates!

## Attribution

Thanks to [Kenney](https://kenney.nl/assets) for the free game assets used during the development (subset of the "Road Textures" and "Racing Pack" collections).

## License

The license covers original work, and excludes anything attributed above. See attribution for third party asset license.

### Source code, custom graphics and sound assets

Copyright 2022 Matias Klemola

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

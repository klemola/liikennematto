![liikennematto_itch_cover_wide](https://user-images.githubusercontent.com/1923450/201537010-ece5c730-ebb9-45e9-b0dc-3ed56e4356b0.png)

## Build the roads and the town comes to life

In Liikennematto (Finnish for "traffic mat") the gameplay is simple: you only build the roads. After a short while, lots appear. With the lots come buildings and their residents. The residents drive around the road network you've created and visit other lots.

https://user-images.githubusercontent.com/1923450/201537214-ddfbad0c-30ee-49e7-9695-f5c8b21e23ff.mp4

### Playful design

Liikennematto is inspired by children's traffic mats. The design of the game follows the traffic mat basics: mixed perspective buildings, plenty of color, clear dark outlines.

Liikennematto is meant for players of all ages and abilities. The UI is free of text and everything's quite large and easy to comprehend.

### Detailed simulation

Cars move and turn smoothly, avoid collision with each other, follow traffic signals and park on lots. Drivers plan their routes ahead and follow them the best they can - rerouting when necessary.

![liikennematto_pathfinding](https://user-images.githubusercontent.com/1923450/201537242-b210f801-4a64-4fcd-9c23-61a12f481f33.png)

🕹️ [Play Liikennematto on itch.io](https://yourmagicisworking.itch.io/liikennematto) 🕹️

## Features

### Simulation

-   a two-lane road network (graph based on the tilemap)
-   a* pathfinding
-   collision prevention
-   signal intersections (traffic lights)
-   yield sign based intersections
-   parking
-   dead end streets
-   procedural generation of lots and buildings
-   debug tools
    -   spawn cars
    -   show road network visualization
    -   show car debug layers

### Map editor

Draw roads of all kind with the smart editor

-   **left click / tap** to add a road tile
-   **right click / long tap** to remove a road tile
-   zoom and pan the map (with touch support)

---

Read more about the project from [Liikennematto dev blog #1: prototyping traffic simulation with Elm](https://matiasklemola.com/liikennematto-dev-blog-one)

[Part #2](https://matiasklemola.com/liikennematto-devlog-two), 
[Part #3](https://matiasklemola.com/liikennematto-devlog-three), 
[Part #4](https://matiasklemola.com/liikennematto-devlog-four)

---

Follow me on [Twitter](https://twitter.com/MatiasKlemola) or [Mastodon](https://mastodon.gamedev.place/@yourmagicisworking) for updates!

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

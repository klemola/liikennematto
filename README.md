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
-   a\* pathfinding
-   collision prevention
-   signal intersections (traffic lights)
-   yield sign based intersections
-   parking
-   procedural generation of lots, buildings and decorations
-   debug tools
    -   spawn cars
    -   show road network visualization
    -   show car debug layers
    -   show lot parking spots and parking lock

### Map editor

Draw roads of all kind with the smart editor

-   **left click / tap** to add a road tile
-   **right click / long tap** to remove a road tile
-   zoom and pan the map (with touch support)

---

Read more about the project from [Liikennematto dev blog #1: prototyping traffic simulation with Elm](https://matiasklemola.com/liikennematto-dev-blog-one)

...or skip to: [Part #2](https://matiasklemola.com/liikennematto-devlog-two) |
[Part #3](https://matiasklemola.com/liikennematto-devlog-three) |
[Part #4](https://matiasklemola.com/liikennematto-devlog-four) |
[Part #5](https://matiasklemola.com/liikennematto-devlog-five)

---

## Instructions for local development

To run Liikennematto, you need `elm 0.19.1`, which you can get from [NPM](https://www.npmjs.com/package/elm) or possibly your system's package manager.

Additionally, you need `elm-format`, `elm-test`, and `elm-review` for devtools, which can all be installed from NPM.

Run `elm reactor` on the project root to start a dev server. You can then navigate to [http://127.0.0.1:8000/src/Main.elm](http://127.0.0.1:8000/src/Main.elm) to build the game. If you make changes, a full reload of the browser window will re-compile it. If you're on a UNIX-based system, you can use `make` to run this command and many others. See [Makefile](./Makefile) for reference on what's possible.

There's one thing, though. I have moved all assets out of the source into a private project that pre-processes and transforms the original assets to work with Liikennematto. As such, if you try to compile Liikennematto, you'll get an error for missing `Assets` module. If you add a dummy module to fix this, you will not be able to render anything meaningful. Sounds will be missing as well. I intend to eventually offer a free asset package for development, but currently, due to time constraints, I can't. Get in touch if you really want to take Liikennematto for a spin.

## Updates

Follow me on [Bluesky](https://bsky.app/profile/matiasklemola.com) or [Mastodon](https://mastodon.gamedev.place/@yourmagicisworking) for updates!

## Attribution

Thanks to [Kenney](https://kenney.nl/assets) for the free game assets used during the development (subset of the "Road Textures" and "Racing Pack" collections).
